# Clearing up workspace
# ---------------------------------------------------------------------------------------------------------------------
rm(list = ls())
source(file = "00_00_Preamble.R")

# Packages needed in this program
# ---------------------------------------------------------------------------------------------------------------------
packages<-c("foreach", "doParallel", "parallel")
check.packages(packages)
# ---------------------------------------------------------------------------------------------------------------------

cl <- makeCluster(10)
registerDoParallel(cl)


CSV_File.list <- c("01_01_Demand_Regression_data.csv", "01_01_Price_Regression_data.csv", "02_01_Reviews_data.csv", "02_01_Reviews_data_plain.csv")
R_Object.list <- c("demand.data", "price.reg.data", "review.data", "review.plain.data")
CSV_File.list <- c("11_00_Demand_Regression_data.RData", "11_00_Price_Regression_data.RData", "11_00_Reviews_data.RData", "11_00_Reviews_data_plain.RData")

assign(R_Object.list[[1]],  read.csv(file = paste(project.path, "Output/TEMP/", CSV_File.list[[1]], sep = ""), 
                                     header = TRUE, 
                                     sep = ",", 
                                     stringsAsFactors = FALSE))




demand.data <- read.csv(file = paste(project.path, "Output/TEMP/", "01_01_Demand_Regression_data.csv", sep = ""), 
                        header = TRUE, 
                        sep = ",", 
                        stringsAsFactors = FALSE)
save(demand.data, file = paste(project.path, "Output/TEMP/", "11_00_Demand_Regression_data.RData", sep = ""))
# ---------------------------------------------------------------------------------------------------------------------
price.reg.data <- read.csv(file = paste(project.path, "Output/TEMP/", "01_01_Price_Regression_data.csv", sep = ""), 
                           header = TRUE, 
                           sep = ",", 
                           stringsAsFactors = FALSE)
save(price.reg.data, file = paste(project.path, "Output/TEMP/", "11_00_Price_Regression_data.RData", sep = ""))
# ---------------------------------------------------------------------------------------------------------------------
review.data <- read.csv(file = paste(project.path, "Output/TEMP/", "02_01_Reviews_data.csv", sep = ""), 
                           header = TRUE, 
                           sep = ",", 
                           stringsAsFactors = FALSE)
save(review.data, file = paste(project.path, "Output/TEMP/", "11_00_Reviews_data.RData", sep = ""))
# ---------------------------------------------------------------------------------------------------------------------
review.plain.data <- read.csv(file = paste(project.path, "Output/TEMP/", "02_01_Reviews_data_plain.csv", sep = ""), 
                           header = TRUE, 
                           sep = ",", 
                           stringsAsFactors = FALSE)
save(review.plain.data, file = paste(project.path, "Output/TEMP/", "11_00_Reviews_data_plain.RData", sep = ""))
# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
