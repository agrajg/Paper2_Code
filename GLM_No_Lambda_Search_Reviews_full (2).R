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

# Begin writing the output to a file
# ---------------------------------------------------------------------------------------------------------------------
output.filename = paste("DML_Output_test", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), sep = "_")
cat('Output sent to : ', '\n')
print(output.filename)
# sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = FALSE)

# Defining parameters 
# ---------------------------------------------------------------------------------------------------------------------
# for h2o cluster
port.num = 11114
max.mem = '800G'

# Loading the data file 
# ---------------------------------------------------------------------------------------------------------------------
ptm.loadRdata <- proc.time()

# Demand data
load(file = paste(project.path,"Output/TEMP/","11_00_Demand_Regression_data.RData", sep=""))
print(demand.data)
# Reviews data
load(file = paste(project.path,"Output/TEMP/","22_01_text_df.RData", sep=""))
# Create rental time panel
print(text.df)
rental.time.panel <-  demand.data %>% 
  mutate(rev_date = date) %>%                   # mutate(rev_date = either booking date or date of stay)
  select(propertyid, date, rev_date) %>%
  as.tbl()
print(rental.time.panel)
gc()
proc.time() - ptm.loadRdata
# =====================================================================================================================

# Take a sample
# ***(this part need to be commented out)***
# # Filter by date
# # ---------------------------------------------------------------------------------------------------------------------
# demand.data <- demand.data %>% 
#   filter(as.Date(date, format = "%d%b%Y") < as.Date("2015-01-01"))
# text.df <- text.df %>% 
#   filter(as.Date(rev_date, format = "%d%b%Y") < as.Date("2015-01-01"))
# rental.time.panel <- rental.time.panel %>% 
#   filter(as.Date(date, format = "%d%b%Y") < as.Date("2015-01-01"))
# 
# Filter by propertyid 
# ---------------------------------------------------------------------------------------------------------------------
# demand.data <- demand.data %>%
#   filter(propertyid >= 0 & propertyid <= 500000)
# text.df <- text.df %>%
#   filter(propertyid >= 0 & propertyid <= 500000)
# rental.time.panel <- rental.time.panel %>%
#   filter(propertyid >= 0 & propertyid <= 500000)
# =====================================================================================================================

# Prepare the data
source(file = "Data_setup.R")
# =====================================================================================================================

# Create varibles needed for DML
source(file = "Creating_relevant_variables.R")
# =====================================================================================================================



K = 5

split     <- h2o.runif(final.df.h2o, seed = 489372)
cvgroup   <- as.numeric(cut(split,quantile(split,probs = seq(0, 1, 1/K)),include.lowest = TRUE)) + 1
j=1
ii  <- cvgroup == j
nii <- cvgroup != j
if(K==1){
  ii  <- cvgroup == j
  nii <- cvgroup == j
}
datause <- final.df.h2o[nii,]
dataout <- final.df.h2o[ii,]


# ===================================================================
# Inputs
datause=datause
dataout=dataout
var.y=Y[1]
var.x=X1
split.ratio= 0.8
num.folds=10
glm_params= list(seed = c(38278),
                 tweedie_variance_power = c(0),
                 tweedie_link_power = c(1), 
                 alpha = c(0.25,0.5, 0.75),
                 lambda = c(0.25,0.5, 2),
                 missing_values_handling = c("Skip"))



start_time <- Sys.time()
# Inputs
datause.split <- NULL
datause.train <- NULL
datause.valid <- NULL
glm_grid <- NULL
glm_grid_perf <- NULL
best_glm <- NULL
yhatout <- NULL
resout <- NULL
train.mse <- NULL
valid.mse <- NULL
test.mse <- NULL


# Splitting data into training and validation frame
datause.split <- h2o.splitFrame(data=datause, ratios=split.ratio)
datause.train <- datause.split[[1]]
datause.valid <- datause.split[[2]]

# Train and validate a cartesian grid of GLMs
time <- Sys.time()
glm_grid <- h2o.grid(algorithm = "glm", 
                     x = var.x, 
                     y = var.y,
                     grid_id = "glm_grid",
                     training_frame = datause.train,
                     validation_frame = datause.valid,
                     hyper_params = glm_params,
                     nfolds = num.folds, 
                     standardize = TRUE, 
                     lambda_search = FALSE
)

cat('Time taken by GLM : ', '\n')
cat('--------------------', '\n')
print(Sys.time() - time)

glm_grid_perf <- h2o.getGrid(grid_id = "glm_grid", sort_by = "mse", decreasing = FALSE)
best_glm <- h2o.getModel(glm_grid_perf@model_ids[[1]])
yhatout <- h2o.predict(best_glm, dataout)
resout <- as.matrix(as.data.frame(dataout[,var.y] - yhatout))
train.mse <- h2o.mse(object = best_glm, train = TRUE, valid = FALSE)
valid.mse <- h2o.mse(object = best_glm, train = FALSE, valid = TRUE)
test.mse <- h2o.mse(h2o.performance(model = best_glm,newdata = dataout))

# Outputs
cat("Grid performance (all hyper-paramenters): ", "\n")
cat("---------------------------------------------------------------------------------------------------------------------------------------", "\n")
print(glm_grid_perf)
cat("=======================================================================================================================================", "\n")

cat("Best Model Summary : ", "\n")
cat("---------------------------------------------------------------------------------------------------------------------------------------", "\n")
print(best_glm@model[["model_summary"]])
cat("=======================================================================================================================================", "\n")

cat("Best Model MSE : ", "\n")
cat("---------------------------------------------------------------------------------------------------------------------------------------", "\n")
cat("On training data : ", train.mse, "\n")
cat("On validation data : ", valid.mse, "\n")
cat("On test data : ", test.mse, "\n")
cat("=======================================================================================================================================", "\n")


end_time <- Sys.time()
h2o.ls()
# Freeing up space
h2o.rm(datause.train)
h2o.rm(datause.valid)
h2o.rm(datause.split)
# h2o.rm(best_glm)
h2o.rm(yhatout)
h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "glm_grid"))
rm(glm_grid)
rm(glm_grid_perf)
gc()
gc()
gc()
h2o.ls()
h2o:::.h2o.garbageCollect()
h2o:::.h2o.garbageCollect()
h2o:::.h2o.garbageCollect()
h2o.ls()

print(head(resout))
print(train.mse)
print(valid.mse)
print(test.mse)
cat('Time taken : ', (end_time - start_time)/1000)

# return(list(resout = resout, 
#             train.mse = train.mse, 
#             valid.mse = valid.mse, 
#             test.mse = test.mse, 
#             run.time = (end_time - start_time)/1000)
#        )

# ===================================================================



h2o.rm(dataout)
h2o.rm(datause)
h2o.rm(ii)
h2o.rm(nii)
h2o.rm(split)
h2o.rm(cvgroup)

gc()
gc()
gc()
h2o.ls()
h2o:::.h2o.garbageCollect()
h2o:::.h2o.garbageCollect()
h2o:::.h2o.garbageCollect()
h2o.ls()

# sink()