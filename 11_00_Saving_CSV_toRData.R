# Importing data from csv to RData
source(file = "00_00_Preamble.R")

demand.data <- read.csv(file = "Output/TEMP/01_01_Demand_Regression_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
save(demand.data, file = "Output/TEMP/11_00_Demand_Regression_data.RData")

price.reg.data <- read.csv(file = "Output/TEMP/01_01_Price_Regression_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
save(price.reg.data, file = "Output/TEMP/11_00_Price_Regression_data.RData")

review.data <- read.csv(file = "Output/TEMP/02_01_Reviews_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
save(review.data, file = "Output/TEMP/11_00_Reviews_data.RData")

review.plain.data <- read.csv(file = "Output/TEMP/02_01_Reviews_data_plain.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
save(review.plain.data, file = "Output/TEMP/11_00_Reviews_data_plain.RData")