cat('Begin Running file : ', '01_01_Reading_CSV_and_Saving_Data.R', " ...", '\n')
cat('--------------------------------------------------------------------', '\n')
{
  cat('R Setup', '\n')
  cat('-------', '\n')
  {
    cat('Clearing up workspace ...', '\n')
    rm(list = ls())
    source(file = "00_00_Preamble.R")
    
    cat('Loading packages needed in this program ...', '\n')
    packages<-c("tidyverse", "tidyr", "dplyr", "data.table", "parallel", "foreach")
    # remove.packages(packages, lib = .libPaths())
    # install.packages(packages, dependencies = TRUE)
    check.packages(packages)
    
    {
      cat('data.table options ...', '\n')
      setDTthreads(threads = (detectCores() - 2 ))
      cat('Data table threads set to ', getDTthreads(), '.', '\n')
    }
    
    file.time <- Sys.time()
    cat('R Setup', '\n')
    cat('=======', '\n')
  }
  
  
  
  
  cat('Reading Demand Data and saving as csv : ', '\n')
  cat('----------------------------------------', '\n')
  {
    time <- Sys.time()
    demand.data <- fread(file = paste(project.path, "Input/", "00_00_Demand_Regression_data.csv", sep = ""),
                         sep = ',', 
                         sep2 = '\t', 
                         stringsAsFactors = FALSE)
    demand.data <- demand.data %>% setDT()
    saveRDS(demand.data, file = paste(project.path, "Output/TEMP/", "01_01_Demand_Regression_data.rds", sep = ""))
    rm(demand.data)
    gc()
    cat('Time taken : ', '\n')
    print(Sys.time() - time)
    cat('Reading Demand Data and saving as csv : ', '\n')
    cat('========================================', '\n')
  }
  
  cat('Reading Reviews Data and saving as csv : ', '\n')
  cat('----------------------------------------', '\n')
  {
    time <- Sys.time()
    reviews.data.plain <- fread(file = paste(project.path, "Input/", "00_00_Reviews_data_plain.csv", sep = ""),
                                sep = ',', 
                                sep2 = '\t', 
                                stringsAsFactors = FALSE)
    reviews.data.plain <- reviews.data.plain %>% setDT()
    saveRDS(reviews.data.plain, file = paste(project.path, "Output/TEMP/", "01_01_Reviews_data_plain.rds", sep = ""))
    rm(reviews.data.plain)
    gc()
    cat('Time taken : ', '\n')
    print(Sys.time() - time)
    cat('Reading Reviews Data and saving as csv : ', '\n')
    cat('========================================', '\n')
  }
  
  cat('Reading Price Data and saving as csv : ', '\n')
  cat('----------------------------------------', '\n')
  {
    time <- Sys.time()
    price.data <- fread(file = paste(project.path, "Input/", "00_00_Price_Regression_data.csv", sep = ""),
                        sep = ',', 
                        sep2 = '\t', 
                        stringsAsFactors = FALSE)
    price.data <- price.data %>% setDT()
    saveRDS(price.data, file = paste(project.path, "Output/TEMP/", "01_01_Price_Regression_data.rds", sep = ""))
    rm(price.data)
    gc()
    cat('Time taken : ', '\n')
    print(Sys.time() - time)
    cat('Reading Price Data and saving as csv : ', '\n')
    cat('========================================', '\n')
  }
  
  cat('End Running file : ', '01_01_Reading_CSV_and_Saving_Data.R', " ...", '\n')
  cat('Time taken : ', '\n')
  print(Sys.time() - file.time)
  cat('==================================================================', '\n')
  
}
