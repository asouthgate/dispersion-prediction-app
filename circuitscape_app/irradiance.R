library(Rcpp)
sourceCpp("circuitscape_app/irradiance.cpp")

wrap_cal_irradiance <- function(lampdf, soft, hard, terr) {

    xmin <- hard@extent@xmin
    xmax <- hard@extent@xmax
    ymin <- hard@extent@ymin
    ymax <- hard@extent@ymax

    hard[is.na(hard[])] <- -1
    soft[is.na(soft[])] <- -1
    terr[is.na(terr[])] <- -1

    irrcpp <- cal_irradiance(as.matrix(lampdf), 
                            as.matrix(soft), as.matrix(hard), as.matrix(terr),
                            xmin, xmax, ymin, ymax)

    irast <- soft
    irast[] <- irrcpp
    return(irast)
}