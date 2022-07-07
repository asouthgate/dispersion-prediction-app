library(rpostgis)
library(logger)

#' wrapper function to make unit testing work with mockr
connect_to_db <- function(driver, db_host, db_name, db_port, db_user, db_pass) {
    connection <- DBI::dbConnect(driver, host=db_host, dbname=db_name, port=db_port, user=db_user, password=db_pass)
    connection
}

#' wrapper function to amke unit testing work with mockr
get_geom <- function(connection, query) {
    geom <- rpostgis::pgGetGeom(connection, query=query)
    geom
}

disconnect_db <- function(connection) {
    DBI::dbDisconnect(connection)
}

#' Build a db query string for getting geometry within an extent
#'
#' @param table_name
#' @param extent a raster::extent object
#' @return query string
build_query_string <- function(table_name, extent) {
    xmin <- attr(extent, "xmin")
    xmax <- attr(extent, "xmax")
    ymin <- attr(extent, "ymin")
    ymax <- attr(extent, "ymax")
    query <- glue("
        SELECT {table_name}.geom
        FROM {table_name}
        WHERE ST_Intersects({table_name}.geom, ST_MakeEnvelope({xmin}, {ymin}, {xmax}, {ymax}, 27700));
    ")
    logger::log_info(paste("got query: ", query))
    return(query)
}

#' Read vector data from a database
#'
#' @param table_name
#' @param extent a raster::extent object
#' @param db_host
#' @param db_name
#' @param db_port
#' @param db_user
#' @param db_pass
#' @return sp-class (SpatialPoints*, SpatialMultiPoints*, SpatialLines*, or SpatialPolygons*)
read_db_vector <- function(table_name, ext, db_host, db_name, db_port, db_user, db_pass) {
    driver <- DBI::dbDriver("PostgreSQL")
    connection <- connect_to_db(driver, db_host, db_name, db_port, db_user, db_pass)
    query <- build_query_string(table_name, ext)
    logger::log_info(paste("Querying db with: ", query))
    # pgGetGeom is badly designed: it calls pgGetGeomQ which calls pgGetGeom again, which
    #   obfuscates errors that are raised
    #   one way around seems to be to capture all output
    tt <- textConnection("redirected_messages", "w")
    sink(tt, type="message")
    results_sf <- tryCatch( {
            results_sf <- get_geom(connection, query)
        },
        error=function(err) {
            logger::log_info("exception occurred:")
            message(err$message)
            logger::log_info("redir message:")
            message(redirected_messages)
            if (grepl("No geometries found", redirected_messages)) {
                # There are no geometries, this should not terminate, just return empty sp df
                results_sf <- sp::SpatialPoints(data.frame(x = 0, y = 0))[-1,]
                return(results_sf)
            }
            else {
                logger::log_info(paste("Going to :@ raise again", err$message))
                # Something else, terminate
                stop(err$message)
            }
        }
    )
    sink(type="message")
    close(tt)
    disconnect_db(connection)
    logger::log_info("Got something from the db, and disconnnected")
    return(results_sf)
}

#' Convert an extent to a vector of values
#'
#' @param ext a raster::extent object
#' @return boundary a vector of values
create_raster_query_boundary <- function(ext) {
    xmin <- attr(ext, "xmin")
    xmax <- attr(ext, "xmax")
    ymin <- attr(ext, "ymin")
    ymax <- attr(ext, "ymax")
    boundary <- c(ymax, ymin, xmax, xmin)
    return(boundary)
}

#' Read a raster from postgis database
#'
#' @param table
#' @param ext a raster::extent object
#' @param db_host
#' @param db_name
#' @param db_port
#' @param db_user
#' @param db_pass
#' @return raster::raster
read_db_raster <- function(table, ext, db_host, db_name, db_port, db_user, db_pass) {
    name <- c("public", table)
    boundary <- create_raster_query_boundary(ext)
    driver <- DBI::dbDriver("PostgreSQL")
    connection <- connect_to_db(driver, db_host, db_name, db_port, db_user, db_pass)

    logger::log_info(paste("Querying raster db with: ", table))

    raster <- rpostgis::pgGetRast(connection, name=name, boundary=boundary)
    DBI::dbDisconnect(connection)
    logger::log_info("got rast and disconnected")

    return(raster)
}

#' Read db raster, return a default if failure, and a boolean flag to indicate failure
read_db_raster_default <- function(table, ext, db_host, db_name, db_port, db_user, db_pass, default) {
    failflag <- FALSE
    raster <- default
    tryCatch(
        {
            raster <- read_db_raster(table, ext, db_host, db_name, db_port, db_user, db_pass) 
        },
        error=function(err) {
            logger::log_warn("Failed to retrieve raster from database!")
            print(err)
            failflag <<- TRUE
        }
    )
    return(list(raster=raster, failflag=failflag))
}