library(Rcpp)
sourceCpp("circuitscape_app/irradiance.cpp")

wrap_cal_irradiance <- function(lampdf, soft, hard, terr, absorbance=0.5, pixw=1, cutoff=100, sensor_ht=0) {

    xmin <- hard@extent@xmin
    xmax <- hard@extent@xmax
    ymin <- hard@extent@ymin
    ymax <- hard@extent@ymax

    hard[is.na(hard[])] <- 0
    soft[is.na(soft[])] <- 0
    terr[is.na(terr[])] <- 0

    irrcpp <- cal_irradiance(as.matrix(lampdf), 
                            as.matrix(soft), as.matrix(hard), as.matrix(terr),
                            xmin, xmax, ymin, ymax,
                            absorbance, pixw, cutoff, sensor_ht)

    irast <- soft
    irast[] <- irrcpp
    return(irast)
}