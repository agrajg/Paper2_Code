cat('* Begin Running file : ', '11_01_Loading_Demand_Review_Data.R', " ...", '\n')
cat('* -------------------------------------------------------------------', '\n')

{
  cat('* R Setup', '\n')
  cat('* -------', '\n')
  {
    source(file = "00_00_Preamble.R")
    
    cat('* Loading packages needed in this program ...', '\n')
    packages<-c("foreach", "doParallel", "parallel", "stringr", "stringi", "tidyverse", "tidyr", "dplyr", "data.table", "magrittr")
    # remove.packages(packages, lib = .libPaths())
    # install.packages(packages, dependencies = TRUE)
    check.packages(packages)
    {
      cat('* data.table options ...', '\n')
      setDTthreads(threads = (detectCores() - 2 ))
      cat('* Data table threads set to ', getDTthreads(), '.', '\n')
    }
    cat('* R Setup', '\n')
    cat('* =======', '\n')
  }
  
  cat('* Loading and preparing the text data', '\n')
  cat('* -----------------------------------', '\n')
  {
    time <- Sys.time()
    review.plain.data <- readRDS(file = paste(project.path,"Output/TEMP/","01_01_Reviews_data_plain.rds", sep=""))
    cat('* Converting date into R date format ... ', '\n')
    review.plain.data$date <- as.Date(review.plain.data$date, "%d%b%Y")
    cat('* Rename date as rev_date variable in reviews data ... ', '\n')
    review.plain.data <- review.plain.data %>% rename(rev_date = date)
    review.plain.data <- review.plain.data %>% setDT
    cat('* Ordering by propertyid and rev_date ... ', '\n')
    review.plain.data <- review.plain.data[order(propertyid, rev_date)]
    cat('* structure of review.plain.data : ', '\n')
    print(str(review.plain.data))
    cat('* Loading and preparing the text data', '\n')
    cat('* Time taken : ', '\n')
    print(Sys.time() - time)
    cat('* ===================================', '\n')
  }
  
  cat('* Loading and preparing the demand data', '\n')
  cat('* -------------------------------------', '\n')
  {
    time <- Sys.time()
    demand.data <- readRDS(file = paste(project.path,"Output/TEMP/","01_01_Demand_Regression_data.rds", sep=""))
    cat('* Converting date into R date format ... ', '\n')
    demand.data$date <- as.Date(demand.data$date, "%d%b%Y")
    cat('* Ordering by propertyid and date ... ', '\n')
    demand.data <- demand.data[order(propertyid, date)]
    cat('* Structure of demand.data : ', '\n')
    print(str(demand.data))
    cat('* Loading and preparing the demand data', '\n')
    cat('* Time taken : ', '\n')
    print(Sys.time() - time)
    cat('* =====================================', '\n')
  }
  
  cat('* End Running file : ', '11_01_Loading_Demand_Review_Data.R', " ...", '\n')
  cat('* Time taken : ', '\n')
  print(Sys.time() - time)
  cat('* ===================================================================', '\n')
}
