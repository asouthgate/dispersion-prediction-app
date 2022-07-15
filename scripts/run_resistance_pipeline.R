source('R/pipeline.R')

# Load Rdata corresponding to user inputs

args <- commandArgs(trailingOnly=TRUE)

load(args[1])
load(args[2])
load(args[3])

logger::log_info(paste("Run resistance script called with:", args[1], args[2], args[3]))

resistance_maps <- cal_resistance_rasters(algorithm_parameters, workingDir, base_inputs, vector_inp, raster_inp, save_images=TRUE)
logger::log_info("Got resistance maps.")

logger::log_info(paste("Saving resistance maps to ", paste0(workingDir, "/resistance_maps.Rdata")))
save(resistance_maps, file=paste0(workingDir, "/resistance_maps.Rdata"))


