library(rpostgis)
library(logger)
library(stringr)

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

read_db_raster_custom <- function(table, ext, db_host, db_name, db_port, db_user, db_pass, resolution=1) {

    minx <- ext@xmin
    maxx <- ext@xmax
    miny <- ext@ymin
    maxy <- ext@ymax

    ncols <- floor( (maxx-minx) / resolution )

    nrows <- floor( (maxy-miny) / resolution )

    logger::log_info(paste(table, db_host, db_name, db_port, db_user, resolution))

    logger::log_info(paste("Ncols/Nrows:", ncols, nrows))
    logger::log_info(paste("Bounds:", minx, maxx, miny, maxy))

    # print(paste(minx, maxx, miny, maxy))
    # print(paste(maxx-minx, maxy-miny))
    # print(paste(nrows, ncols))

    logger::log_debug("building string")

    logger::log_info(paste0(minx, maxx, miny, maxy))

    env_string <- paste0('ST_MakeEnvelope(', minx, ',', miny, ',', maxx, ',', maxy, ', 27700)')
    logger::log_info(env_string)

    env_string <- paste0("ST_SetSRID(ST_GeomFromText($$POLYGON((", 
        minx, " ", maxy, ",", 
        minx, " ", miny, ",",
        maxx, " ", miny, ",",
        maxx, " ", maxy, ",",
        minx, " ", maxy, "))$$), 27700)")

    logger::log_info(env_string)

    # if (resolution > 1) {
    logger::log_info("big str")
    # str <- paste0('select unnest(st_dumpvalues(ST_Resample(ST_Clip(rast,', 
    #                 env_string,
    #                 '), ', 
    #                 ncols, ', ', nrows, ', ', maxx, ', ', miny, 
    #                 '), 1)) as vals from (select st_union(\"rast\",1) rast from \"public\".\"', 
    #                 table, '\" WHERE ST_Intersects(\"rast\",', env_string, ')) as a;')

    transformstr <- paste0('ST_Resample(ST_Clip(ST_Union(\"rast\", 1),', env_string, '), ', 
                                    ncols, ', ', nrows, ', ', maxx, ', ', miny, 
                                ')')

    print(transformstr)

    selectstr <- paste0('unnest(ST_dumpvalues(', transformstr, ', 1))')

    print(selectstr)

    # selectstr <- 'rast'

    str <- paste0('select ', selectstr, ' from \"public\".\"', 
                    table, '\" WHERE ST_Intersects(\"rast\",', env_string, ');')

    logger::log_info(str)
    metastr <- paste0('select ',
                    'st_xmax(st_envelope(rast)) as xmx, ',
                    'st_xmin(st_envelope(rast)) as xmn, ',
                    'st_ymax(st_envelope(rast)) as ymx, ',
                    'st_ymin(st_envelope(rast)) as ymn ',
        ' from (select ', transformstr, ' rast from \"public\".\"', 
        table, '\" WHERE ST_Intersects(\"rast\",', env_string, ')) as a;')
    # } else {
    #     str <- paste0('select unnest(st_dumpvalues(ST_Clip(rast,',env_string ,'), 1)) as vals from (select st_union(\"rast\",1) rast from \"public\".\"', 
    #         table, '\" WHERE ST_Intersects(\"rast\",', env_string, ')) as a;')
    #     metastr <- paste0('select st_width(rast) as cols, ', 
    #                     'st_height(rast) as rows, ',
    #                     'st_xmax(st_envelope(rast)) as xmx, ',
    #                     'st_xmin(st_envelope(rast)) as xmn, ',
    #                     'st_ymax(st_envelope(rast)) as ymx, ',
    #                     'st_ymin(st_envelope(rast)) as ymn ',
    #         ' from (select st_union(\"rast\",1) rast from \"public\".\"', 
    #         table, '\" WHERE ST_Intersects(\"rast\",', env_string, ')) as a;')
    # }

    logger::log_debug(str)
    call <- paste0("PGPASSWORD=", db_pass, " psql -U ", db_user, 
        " -d ", db_name, " -h ",
        db_host, " -p ", db_port, " -P pager=off -c \'", str, "\'")
    logger::log_debug(call)
    print(call)
    print("????")

    vals <- str_split(system(call, intern = TRUE), "\n")
    # print(str)

    # if (resolution < 2) {

    metacall <- paste0("PGPASSWORD=", db_pass, " psql -U ", db_user, 
        " -d ", db_name, " -h ",
        db_host, " -p ", db_port, " -P pager=off -c \'", metastr, "\'")

    metavals <- str_split(system(metacall, intern=TRUE), "\n")[[3]]
    # print("metaavals")
    # print(metavals)
    metavals <- as.numeric(str_split(metavals, "\\|")[[1]])
    # print(metavals)
    # ncols <- as.numeric(metavals[1])
    # nrows <- as.numeric(metavals[2])
    # print(paste(ncols, nrows))
    # }

    # print(vals[1:100]

    L <- length(vals)
    print(L)
    vals <- as.numeric(vals[3:(L-2)])

    print(length(vals))
    print(nrows)
    print(ncols)
    print(nrows * ncols)

    print(metavals)

    dbmaxx <- metavals[1]
    dbminx <- metavals[2]
    dbmaxy <- metavals[3]
    dbminy <- metavals[4]
    
    r <- raster::raster(nrows=nrows, ncols=ncols, xmn=dbminx, xmx=dbmaxx, ymn=dbminy, ymx=dbmaxy, crs=sp::CRS("+init=epsg:27700"))
    raster::values(r) <- vals

    raster::crop(r, ext)

    r
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
    print(boundary)

    raster <- rpostgis::pgGetRast(connection, name=name, boundary=boundary)
    DBI::dbDisconnect(connection)
    logger::log_info("got rast and disconnected")

    return(raster)
}

#' Read db raster, return a default if failure, and a boolean flag to indicate failure
read_db_raster_default <- function(table, ext, db_host, db_name, db_port, db_user, db_pass, default, resolution=NULL) {
    failflag <- FALSE
    raster <- default
    tryCatch(
        {
            # raster <- read_db_raster(table, ext, db_host, db_name, db_port, db_user, db_pass) 
            raster <- read_db_raster_custom(table, ext, db_host, db_name, db_port, db_user, db_pass, resolution=resolution) 
        },
        error=function(err) {
            logger::log_warn("Failed to retrieve raster from database!")
            # logger::log_warn(err)
            logger::log_warn(err$message)
            failflag <<- TRUE
        }
    )
    print(failflag)
    return(list(raster=raster, failflag=failflag))
}