source('R/pipeline.R')

# Load Rdata corresponding to user inputs

args = commandArgs(trailingOnly=TRUE)

working_dir <- args[1]

l_map <- call_circuitscape(working_dir, TRUE)
logger::log_info("Got current map.")
