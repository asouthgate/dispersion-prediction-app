library(rpostgis)


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
    print(paste("got query: ", query))
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
    connection <- DBI::dbConnect(driver,
                                host=db_host,
                                dbname=db_name,
                                port=db_port,
                                user=db_user,
                                password=db_pass)
    query <- build_query_string(table_name, ext)
    print(paste("Querying db with: ", query))
    results_sf <- rpostgis::pgGetGeom(connection, query=query)
    DBI::dbDisconnect(connection)
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
    connection <- DBI::dbConnect(driver,
                            host=db_host,
                            dbname=db_name,
                            port=db_port,
                            user=db_user,
                            password=db_pass)
    raster <- rpostgis::pgGetRast(connection, name=name, boundary=boundary)
    DBI::dbDisconnect(connection)
    return(raster)
}