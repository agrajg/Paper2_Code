source('Code/00_00_Preamble.R', echo=TRUE)
# Testing h2o 
library(h2o)
# Start H2O on your local machine using all available cores
# (By default, CRAN policies limit use to only 2 cores)
h2o.init(ip = "localhost", port = 54321, nthreads= -1, max_mem_size = "1000g")
#Get help
?h2o.glm
?h2o.gbm

# Show a demo
# demo(h2o.glm)
# demo(h2o.gbm)


# Import dataset and display summary

# library(nycflights13)
# airlinesURL = "https://s3.amazonaws.com/h2o-airlinesunpacked/allyears2k.csv"
# airlines.hex = h2o.importFile(path = airlinesURL, destination_frame = "airlines.hex")
airlines.hex = as.h2o(flights, destination_frame = "airlines.hex")
#To import small iris data file from H2O’s package:
irisPath = system.file("extdata", "iris.csv", package="h2o")
iris.hex = h2o.importFile(path = irisPath, destination_frame = "iris.hex")



# # To import an entire folder of files as one data object:
# pathToFolder = "/Users/data/airlines/"
# airlines.hex = h2o.importFile(path = pathToFolder,destination_frame = "airlines.hex")
# 
# # To import from HDFS and connect to H2O in R using the IP and port of an H2O instance running on your Hadoop cluster:
# # h2o.init(ip= <IPAddress>, port =54321, nthreads =-1)
# pathToData = "hdfs://mr-0xd6.h2oai.loc/datasets/airlines_all.csv"
# airlines.hex = h2o.importFile(path = pathToData, destination_frame = "airlines.hex")

irisPath = system.file("extdata", "iris.csv", package="h2o")
iris.hex = h2o.uploadFile(path = irisPath, destination_frame = "iris.hex")
irisPath = system.file("extdata", "iris_wheader.csv", package="h2o")
iris.hex = h2o.importFile(path = irisPath)
h2o.anyFactor(iris.hex)

prosPath <- system.file("extdata", "prostate.csv",package="h2o")
prostate.hex <- h2o.importFile(path = prosPath)
as.factor(prostate.hex[,4])

h2o.shutdown(prompt = TRUE)
