library(raster)

load('./test/base_inputs.Rdata')
load('./test/input_data.Rdata')
load('./test/resistance_maps.Rdata')

source('./R/resistance.R')
source('./R/rasterfunc.R')
source('./R/pipeline.R')


x <- algorithmParameters$roost$x
y <- algorithmParameters$roost$y
r <- algorithmParameters$roost$radius



for (n in c(1, 2, 5, 10, 50, 200)) {
	workingDir <- paste0('./test/tmp/multi_circles', n)
	dir.create(workingDir)
	dir.create(paste0(workingDir, '/circuitscape'))

	    writeRaster(
		base_inputs$groundrast,
		paste0(workingDir, "/circuitscape/ground.asc"),
		overwrite=TRUE
	    ) # TODO: Create a random filename for each request


	circles <- create_circles(base_inputs$groundrast, x, y, r, n)
	base_inputs$circles <- circles

	resistance_maps <- cal_resistance_rasters(algorithmParameters, workingDir, base_inputs, shinyProgress, progressMax, save_images=TRUE)
	prepare_circuitscape_ini_file(workingDir)
	call_circuitscape(workingDir, save_images=TRUE)
}

