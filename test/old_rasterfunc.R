lightdist <- function(xdist,ydist,zdist,theta=NULL) {
    xyzdist <- sqrt(xdist^2+ydist^2+zdist^2)
    return(1/(xyzdist^2))
}

calc_point_irradiance_old <- function(lamps, soft_surf, hard_surf, terrain) {
  point_irradiance <- soft_surf
  values(point_irradiance) <- 0 
  ext <- 100
  sensor_ht <- 2.5
  absorbance <- 0.5  
  for(lamp in 1:dim(lamps)[1]) {
    
    x <- lamps$x[lamp]
    y <- lamps$y[lamp]
    z <- lamps$z[lamp]

    # print(paste(x,y,z))
    # print(paste("ymin hard surf", ymin(hard_surf)))
    # print(paste("ymax hard surf", ymax(hard_surf)))
    # print(hard_surf)

    ### find dimensions of area of influence of light
    row <- rowFromY(hard_surf,min(max(y,ymin(hard_surf)),ymax(hard_surf)))
    col <- colFromX(hard_surf,max(min(x,xmax(hard_surf)),xmin(hard_surf)))
    col_min <- max(col-ext,1)
    col_max <- min(col+ext,colFromX(hard_surf,xmax(hard_surf)))
    col_lamp <- col-col_min
    row_min <- max(row-ext,1)
    row_max <- min(row+ext,rowFromY(hard_surf,ymin(hard_surf)))
    row_lamp <- row-row_min
    nrows <- row_max-row_min
    ncols <- col_max-col_min

    # print(paste(col_min, col_max))
    # print(paste(row_min, row_max))
    # print(paste(row_lamp, col_lamp))
    # print(paste(nrows, ncols))


    ### extract terrain data for area of influence of light 
    hard_block <- array(getValuesBlock(hard_surf,row=row_min,nrows=nrows,col=col_min,ncols=ncols),c(nrows,ncols))
    soft_block <- array(getValuesBlock(soft_surf,row=row_min,nrows=nrows,col=col_min,ncols=ncols),c(nrows,ncols))
    terrain_block <- array(getValuesBlock(terrain,row=row_min,nrows=nrows,col=col_min,ncols=ncols),c(nrows,ncols))
    hard_block[is.na(hard_block==TRUE)] <- 0
    soft_block[is.na(soft_block==TRUE)] <- 0
    terrain_block[is.na(terrain_block==TRUE)] <- 0
    point_irrad <- array(0,c(ncols,nrows))
    ### find values for irradiance on a horizontal plane and sphere
    # if (TRUE) {
    if (ncols==200&nrows==200) {
        # print("enough cols and rows")

        # print(paste("ri_lamp, cj_lamp, z, nrows, ncols, ext", row_lamp, col_lamp, z, nrows, ncols, ext))
        # print(paste(dim(terrain_block),":", max(terrain_block)))
        # print(paste(dim(hard_block),":", max(hard_block)))
        # print(paste(dim(soft_block),":", max(soft_block)))

      for(xx in 1:ncols) {
        for (yy in 1:nrows) {
          xdist <- col_lamp-xx
          ydist <- row_lamp-yy
          xydist <- sqrt(xdist^2+ydist^2)
          zdist <- (terrain_block[row_lamp,col_lamp]+z)-(terrain_block[yy,xx]+sensor_ht)
          xyzdist <- sqrt(xydist^2+zdist^2)
          dist <- floor(xydist+0.5)
        #   print(paste("Distance from lamp", dist))
        #   print(paste("ext, zdist, hardvals", ext, hard_block[yy,xx]))
          if (xydist<=ext && zdist>0 && is.na(hard_block[yy,xx])==FALSE && dist>0) {
            shadow <- 1
            shading <- 0
            for (d in 1:dist) {
              if(hard_block[as.integer(yy+(ydist)*(d/dist)),as.integer(xx+(xdist)*(d/dist))] >= (terrain_block[yy,xx]+sensor_ht+(d/dist)*zdist)) {
                shadow <- 0
                break
              }
              if(soft_block[as.integer(yy+(ydist)*(d/dist)),as.integer(xx+(xdist)*(d/dist))] >= (terrain_block[yy,xx]+sensor_ht+(d/dist)*zdist)) {
                shading <- shading + xyzdist/xydist
              }
            #   print(paste("shadow, shading", shadow, shading))
              point_irrad[xx,yy] <- (1/(10^(absorbance*shading)))*shadow*lightdist(xdist,ydist,zdist)
            }
          }
        }
      }
      point_values <- raster(point_irrad,xmn=xFromCol(point_irradiance,col_min),ymn=yFromRow(point_irradiance,row_max),xmx=xFromCol(point_irradiance,col_max),ymx=yFromRow(point_irradiance,row_min))
      origin(point_values) <- origin(point_irradiance)
      point_irradiance <- mosaic(point_irradiance,point_values,fun=sum)
      removeTmpFiles()
    }
  }
#   writeRaster(point_irradiance,filename=paste("point_irradiance.tif"),overwrite=TRUE)
  return(point_irradiance)
}