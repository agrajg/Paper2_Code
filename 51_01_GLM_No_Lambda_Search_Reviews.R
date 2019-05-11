# Clearing up workspace
# ---------------------------------------------------------------------------------------------------------------------
rm(list = ls())
source(file = "00_00_Preamble.R")
# =====================================================================================================================
# Packages needed in this program
# ---------------------------------------------------------------------------------------------------------------------
packages<-c("ggplot2", "foreach", "doParallel", "h2o", "stringr", "stringi", "gmm", "tidyverse", "tidyr", "dplyr")
# remove.packages(packages, lib = .libPaths())
check.packages(packages)
# =====================================================================================================================
# for h2o cluster
port.num = 11114
max.mem = '1000G'
source(file = "DML_Function.R") # Here we define all the functions used in this program
source(file = "Loading_the_data_file.R") # Loading the data file 
source(file = "Take_a_sample.R")     # Take a sample     # *** (this part need to be commented out) ***
source(file = "Data_setup.R") # Prepare the data
source(file = "Creating_relevant_variables.R")   # Creating relevant variables
# =====================================================================================================================
# MAIN PROGRAM
# =====================================================================================================================
# Take all parmeters as inputs
file.num = "51_01"
method = "GLM_No_lambda_Search"
is.test = "full"
# Defining parameters 
# ---------------------------------------------------------------------------------------------------------------------
# for ML algorithms
cv.num.folds=0        # *** Change this in the final program
times.split.grid = 1         # *** Change this in the final program ***
grid = "alpha_0_pt5_pt99_1_lambda_pt001_pt01_pt1_1_10"
glm_params_no_lambda_search = list(alpha = c(0, 0.5, 0.99, 1),lambda = c(0.001,0.01,0.1,1,10))  # ***Remove this in the final program*** 
# For DML
times.split.dml = 2
crossfit.K = 2
output.filename = paste(file.num,"DML_Output",is.test, method , grid, "CVfold", as.character(cv.num.folds),"grsp",as.character(times.split.grid),
                        "dmlsp", as.character(times.split.dml),"dmlK", as.character(crossfit.K),sep = "_")
cat('Output sent to : ', '\n')
print(output.filename)
sink(file = paste(project.path,"Output/Final/",output.filename,".txt", sep=""), append = FALSE)    # *** Keep in the final run ***
cat('This file : ', '\n')
print(output.filename)
# var.x = X[1:30]                     # *** Remove this in the final program***
var.x = X                         # ***Keep this in the final program******


# Z <- setdiff(str_subset(final.df.h2o %>% h2o.colnames(), "prod_week") , c("proddum","prod_week1", "prod_week2", "prod_week3", "prod_week4", "prod_week5"))
# Z <- Z[1:4]                     # *** Remove this in the final program***

ML_outcome.list  <- c(Y,D,Z)      
# ---------------------------------------------------------------------------------------------------------------------
# Printing 
cat('cv.num.folds = ', cv.num.folds, '\n')
cat('times.split.grid = ', times.split.grid, '\n')
cat('grid = ', grid, '\n')
cat('glm_params_no_lambda_search = ', '\n') 
glm_params_no_lambda_search %>% print()
cat('times.split.dml = ', times.split.dml, '\n')
cat('crossfit.K = ', crossfit.K, '\n')
cat('var.x = ', '\n')
var.x %>% print()
cat('ML_outcome.list = ', '\n')
ML_outcome.list %>% print()
# =====================================================================================================================
# Pick best GLM model without lambda search
# ---------------------------------------------------------------------------------------------------------------------
# *** This can be commented if this has been run already run ***
source(file = "Pick_best_GLM_model_without_lambda_search.R")
# Load file containing hyper parameter values to be used
load(file = paste(project.path,"Output/Final/", output.filename ,"GridSearchResults.RData", sep=""))
cat('best Glm model without lambda search : ', '\n')
best_glm_grid_no_lambda_search_df %>% print()
# =====================================================================================================================
# Apply DML with best GLM without lambda search
# ---------------------------------------------------------------------------------------------------------------------
source(file = "Apply_DML_with_best_GLM_without_lambda_search.R")
# =====================================================================================================================

sink()                                                                                        # *** Keep in the final run ***
