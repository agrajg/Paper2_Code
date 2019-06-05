{
  cat('* Clearing up workspace ...', '\n')
  rm(list = ls())
  source(file = "00_00_Preamble.R")
  
  cat('* Loading packages needed in this program ...', '\n')
  packages<-c("slam","foreach", "doParallel", "parallel", "stringr", "stringi", "gmm", "tidyr", "dplyr", "data.table", "ggplot2")
  # remove.packages(packages, lib = .libPaths())
  # install.packages(packages, dependencies = TRUE)
  check.packages(packages)
  
  file.time <- Sys.time()
}