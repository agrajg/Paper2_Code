# Clearing up workspace
# ---------------------------------------------------------------------------------------------------------------------
rm(list = ls())
source(file = "00_00_Preamble.R")

# Packages needed in this program
# ---------------------------------------------------------------------------------------------------------------------
packages<-c("ggplot2", "foreach", "doParallel", "dplyr", "caret", "h2o")
check.packages(packages)

h2o.init(port = 11111, nthreads = -1, max_mem_size = '1000G')


# Loading the data file 
# ---------------------------------------------------------------------------------------------------------------------
load(file = paste(project.path,"Output/TEMP/","11_00_Demand_Regression_data.RData", sep=""))




start_time <- Sys.time()
demand.data.h2o <- as.h2o(demand.data)
end_time <- Sys.time()

end_time - start_time


start_time <- Sys.time()
demand.data.back <- as.data.frame(demand.data.h2o)
end_time <- Sys.time()

end_time - start_time