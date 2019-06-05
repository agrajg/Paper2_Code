# Clearing up workspace
# ---------------------------------------------------------------------------------------------------------------------
rm(list = ls())
source(file = "00_00_Preamble.R")

# Packages needed in this program
# ---------------------------------------------------------------------------------------------------------------------
packages<-c("ggplot2", "foreach", "doParallel", "caret", "h2o", "stringr", "gmm", "tidyverse", "tidyr", "dplyr")
check.packages(packages)
# ---------------------------------------------------------------------------------------------------------------------
demand.data <- read.csv(file = paste(project.path, "Output/TEMP/", "01_01_Demand_Regression_data.csv", sep = ""), 
                        header = TRUE, 
                        sep = ",", 
                        stringsAsFactors = FALSE)

save(demand.data, file = paste(project.path, "Output/TEMP/", "11_00_Demand_Regression_data.RData", sep = ""))
# ---------------------------------------------------------------------------------------------------------------------
review.plain.data <- read.csv(file = paste(project.path, "Output/TEMP/", "02_01_Reviews_data_plain.csv", sep = ""), 
                              header = TRUE, 
                              sep = ",", 
                              stringsAsFactors = FALSE)
save(review.plain.data, file = paste(project.path, "Output/TEMP/", "11_00_Reviews_data_plain.RData", sep = ""))
# ---------------------------------------------------------------------------------------------------------------------
price.reg.data <- read.csv(file = paste(project.path, "Output/TEMP/", "01_01_Price_Regression_data.csv", sep = ""), 
                           header = TRUE, 
                           sep = ",", 
                           stringsAsFactors = FALSE)
save(price.reg.data, file = paste(project.path, "Output/TEMP/", "11_00_Price_Regression_data.RData", sep = ""))
# ---------------------------------------------------------------------------------------------------------------------

# # ---------------------------------------------------------------------------------------------------------------------
# review.data <- read.csv(file = paste(project.path, "Output/TEMP/", "02_01_Reviews_data_20909.csv", sep = ""), 
#                         header = TRUE, 
#                         sep = ",", 
#                         stringsAsFactors = FALSE)
# # enc2utf8(review.data$corpus)
# save(review.data, file = paste(project.path, "Output/TEMP/", "11_00_Reviews_data_20909.RData", sep = ""))
# # ---------------------------------------------------------------------------------------------------------------------



# # ---------------------------------------------------------------------------------------------------------------------
# review.data <- read.csv(file = paste(project.path, "Output/TEMP/", "02_01_Reviews_data.csv", sep = ""), 
#                         header = TRUE, 
#                         sep = ",", 
#                         stringsAsFactors = FALSE)
# # enc2utf8(review.data$corpus)
# save(review.data, file = paste(project.path, "Output/TEMP/", "11_00_Reviews_data.RData", sep = ""))
# # ---------------------------------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------------------------

