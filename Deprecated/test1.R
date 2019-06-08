# Clearing up workspace
# ---------------------------------------------------------------------------------------------------------------------
rm(list = ls())
source(file = "00_00_Preamble.R")

# Packages needed in this program
# ---------------------------------------------------------------------------------------------------------------------
packages<-c("ggplot2", "foreach", "doParallel", "dplyr", "caret", "h2o", "stringr", "gmm")
check.packages(packages)
# =====================================================================================================================


# Loading the data file 
# ---------------------------------------------------------------------------------------------------------------------
ptm.loadRdata <- proc.time()
load(file = paste(project.path,"Output/TEMP/","11_00_Demand_Regression_data.RData", sep=""))
proc.time() - ptm.loadRdata
# =====================================================================================================================


# Setting things in H2O environment and loading the entire data on to H2O environment.
# This may take about 20 minutes
# ---------------------------------------------------------------------------------------------------------------------
for (i in c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1)){
# i=0.001
  h2o.init(port = 11111, nthreads = -1, max_mem_size = '1000G')
  h2o.removeAll() ## clean slate - just in case the cluster was already running
  # Take a sample (this part need to be commented out)
  # ---------------------------------------------------------------------------------------------------------------------
  # demand.data.2 <- demand.data
  demand.data1 <- sample_frac(demand.data, size = i)
  ptm.loadH2Odata <- proc.time()
  demand.data.h2o <- as.h2o(demand.data)
  cat("Time taken by ", (i*100), " % of data = ")
  print(proc.time() - ptm.loadH2Odata)
  h2o.shutdown(prompt = FALSE)
  # =====================================================================================================================
}

  
  
# =====================================================================================================================
