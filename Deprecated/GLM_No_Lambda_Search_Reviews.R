# Clearing up workspace
# ---------------------------------------------------------------------------------------------------------------------
rm(list = ls())
source(file = "00_00_Preamble.R")

# Packages needed in this program
# ---------------------------------------------------------------------------------------------------------------------
packages<-c("ggplot2", "foreach", "doParallel", "caret", "h2o", "stringr", "gmm", "tidyverse", "tidyr", "dplyr")
# remove.packages(packages, lib = .libPaths())
check.packages(packages)
# =====================================================================================================================

# Setting things in H2O environment and loading the entire data on to H2O environment.
# This may take about 20 minutes
# ---------------------------------------------------------------------------------------------------------------------
h2o.init(port = 11112, nthreads = -1, max_mem_size = '600G')
h2o.removeAll() ## clean slate - just in case the cluster was already running
h2o.ls()

# Loading the data file 
# ---------------------------------------------------------------------------------------------------------------------
ptm.loadRdata <- proc.time()

# Demand data
load(file = paste(project.path,"Output/TEMP/","11_00_Demand_Regression_data.RData", sep=""))
# Reviews data
load(file = paste(project.path,"Output/TEMP/","22_01_text_df.RData", sep=""))
# Create rental time panel
rental.time.panel <-  demand.data %>%             # mutate(rev_date = either booking date or date of stay)
  select(propertyid, date, rev_date) %>%
  as.tbl()

gc()
proc.time() - ptm.loadRdata
# =====================================================================================================================

# Take a sample
# ***(this part need to be commented out)***
# ---------------------------------------------------------------------------------------------------------------------
demand.data <- demand.data %>% 
  filter(date < as.Date("2015-01-01"))
text.df <- text.df %>% 
  filter(rev_date < as.Date("2015-01-01"))
rental.time.panel <- rental.time.panel %>% 
  filter(date < as.Date("2015-01-01"))
# =====================================================================================================================


ptm.loadH2Odata <- proc.time()
demand.data.h2o <- as.h2o(demand.data)
proc.time() - ptm.loadH2Odata
h2o.ls()
ptm.loadH2Odata <- proc.time()
text.df.h2o <- as.h2o(text.df)
proc.time() - ptm.loadH2Odata
h2o.ls()
ptm.loadH2Odata <- proc.time()
rental.time.panel.h2o <- as.h2o(rental.time.panel)
# rental.time.panel.h2o$date <- h2o.as_date(rental.time.panel.h2o$date, format="%d%b%Y")
proc.time() - ptm.loadH2Odata
h2o.ls()
# =====================================================================================================================

# perform merge properly
# ---------------------------------------------------------------------------------------------------------------------
time <- Sys.time()
cat('Run begin ...', '\n')
m1 <- h2o.merge(rental.time.panel.h2o,text.df.h2o, all.x =  TRUE)
m2 <- h2o.merge(rental.time.panel.h2o,text.df.h2o, all.y =  TRUE)
mask <- is.na(m2[,"date"])
cols <- m2[mask,]
h2o.dim(cols)
tempdata.h2o <- h2o.rbind(m1, cols)
cat('SAMPLE : ' , '\n')
print(h2o.dim(tempdata.h2o))
print(tempdata.h2o[1:10,1:10])
h2o.ls()
cat('Run complete.', '\n')
cat('Time taken : ', '\n')
print(Sys.time() - time)
# Clean up 
h2o.rm(m1)
h2o.rm(m2)
h2o.rm(mask)
h2o.rm(cols)
h2o:::.h2o.garbageCollect()
h2o:::.h2o.garbageCollect()
h2o:::.h2o.garbageCollect()
h2o.ls()

# =====================================================================================================================


# =====================================================================================================================
time <- Sys.time()
cat('Run begin ...', '\n')
# text.panel.h2o <- h2o.arrange(tempdata.h2o , propertyid, date)
tempdata.h2o$date <- h2o.ifelse(is.na(tempdata.h2o$date), tempdata.h2o$rev_date, tempdata.h2o$date)
text.panel.h2o <- h2o.rank_within_group_by(tempdata.h2o, 
                                           group_by_cols = c("propertyid"), 
                                           sort_cols =c("date"), 
                                           ascending = NULL,
                                           new_col_name = "New_Rank_column", 
                                           sort_cols_sorted = TRUE)
h2o.ls()
cat('SAMPLE : ' , '\n')
print(h2o.dim(text.panel.h2o))
print(text.panel.h2o[1:10,1:10])
cat('Run complete.', '\n')
cat('Time taken : ', '\n')
print(Sys.time() - time)

# =====================================================================================================================

# h2o.rm(tempdata.h2o)
# text.panel.h2o$pid = h2o.difflag1(text.panel.h2o$propertyid)
# print(sum(text.panel.h2o$pid, na.rm = TRUE))
# text.panel.h2o$pid <- h2o.ifelse(is.na(text.panel.h2o$pid), 1, text.panel.h2o$pid)
# print(sum(text.panel.h2o$pid, na.rm = TRUE))   # should be one more than previous (its a test)


text_colnames <- setdiff(h2o.colnames(text.panel.h2o),h2o.colnames(rental.time.panel.h2o))
text_colnames <- setdiff(text_colnames,c("New_Rank_column"))

time <- Sys.time()
cat('Run begin ...', '\n')
for(i in text_colnames){
  text.panel.h2o[,i] = ifelse(text.panel.h2o[, "New_Rank_column"] ==1 & is.na(text.panel.h2o[,i]), 0, text.panel.h2o[,i])
}
cat('Run complete.', '\n')
cat('Time taken : ', '\n')
print(Sys.time() - time)


br..sdld

# Making data nice and ready
# ---------------------------------------------------------------------------------------------------------------------
# Y
demand.data.h2o$qdemand <- as.numeric(demand.data.h2o$qdemand)
# D
demand.data.h2o$lprice_per_person <- as.numeric(demand.data.h2o$lprice_per_person)

# Z
demand.data.h2o$prod_week1 <- as.numeric(demand.data.h2o$prod_week1)
demand.data.h2o$prod_week2 <- as.numeric(demand.data.h2o$prod_week2)
demand.data.h2o$prod_week3 <- as.numeric(demand.data.h2o$prod_week3)
demand.data.h2o$prod_week4 <- as.numeric(demand.data.h2o$prod_week4)
demand.data.h2o$prod_week5 <- as.numeric(demand.data.h2o$prod_week5)

# X
demand.data.h2o$qdemand_l1 <- as.numeric(demand.data.h2o$qdemand_l1)
demand.data.h2o$qdemand_l2 <- as.numeric(demand.data.h2o$qdemand_l2)
demand.data.h2o$qdemand_l3 <- as.numeric(demand.data.h2o$qdemand_l3)

demand.data.h2o$listingtype <- as.factor(demand.data.h2o$listingtype)   
demand.data.h2o$bedrooms <- as.factor(demand.data.h2o$bedrooms) 
demand.data.h2o$bathrooms <- as.factor(demand.data.h2o$bathrooms) 
demand.data.h2o$nbhd <- as.factor(demand.data.h2o$nbhd)
demand.data.h2o$latitude <- as.numeric(demand.data.h2o$latitude)
demand.data.h2o$longitude <-  as.numeric(demand.data.h2o$longitude)
demand.data.h2o$p_age <- as.numeric(demand.data.h2o$p_age)
demand.data.h2o$h_age <- as.numeric(demand.data.h2o$h_age)
demand.data.h2o$p_dayshosting <- as.numeric(demand.data.h2o$p_dayshosting)
demand.data.h2o$h_dayshosting <- as.numeric(demand.data.h2o$h_dayshosting)
demand.data.h2o$p_daysbooked <- as.numeric(demand.data.h2o$p_daysbooked)
demand.data.h2o$h_daysbooked <- as.numeric(demand.data.h2o$h_daysbooked)
demand.data.h2o$p_guestcount <- as.numeric(demand.data.h2o$p_guestcount)
demand.data.h2o$h_guestcount <- as.numeric(demand.data.h2o$h_guestcount)
# Rental ID
demand.data.h2o$propertyid <- as.factor(demand.data.h2o$propertyid)

# handling date variables
demand.data.h2o$date <- h2o.as_date(demand.data.h2o$date, format="%d%b%Y")
demand.data.h2o$week <- as.factor(h2o.week(demand.data.h2o$date))
demand.data.h2o$year <- as.factor(h2o.year(demand.data.h2o$date))
demand.data.h2o$dayOfWeek <- as.factor(h2o.dayOfWeek(demand.data.h2o$date))
demand.data.h2o$date <- as.factor(demand.data.h2o$date)
# Interactions
demand.data.h2o$year_week_nbhd <- as.factor(h2o.interaction(demand.data.h2o, 
                                                            factors = list(c("year", "week", "nbhd")), 
                                                            pairwise = FALSE, 
                                                            max_factors = 1000000, 
                                                            min_occurrence = 1))

demand.data.h2o$year_dayOfWeek_nbhd <- as.factor(h2o.interaction(demand.data.h2o, 
                                                                 factors = list(c("year", "dayOfWeek", "nbhd")), 
                                                                 pairwise = FALSE, 
                                                                 max_factors = 1000000, 
                                                                 min_occurrence = 1))
# =====================================================================================================================

# Final data check
# ---------------------------------------------------------------------------------------------------------------------
# head(demand.data.h2o)
# class(demand.data.h2o)
dim(demand.data.h2o)

# =====================================================================================================================

# Defining input output and other parameters
# ---------------------------------------------------------------------------------------------------------------------
# Y
Y <- c("qdemand")

# D
D <- c("lprice_per_person", "lprice")

# Z
Z <- c("prod_week1", "prod_week2", "prod_week3", "prod_week4", "prod_week5")

# X
X <- c("qdemand_l1","qdemand_l2" ,"qdemand_l3", 
       "listingtype", "bedrooms", "bathrooms", "nbhd",
       "latitude","longitude", 
       "p_age", "h_age" ,"p_dayshosting" ,"h_dayshosting" ,
       "p_daysbooked" ,"h_daysbooked", 
       "p_guestcount", "h_guestcount",
       "propertyid", "date", "year_week_nbhd","year_dayOfWeek_nbhd")

# =====================================================================================================================


K = 5
split     <- h2o.runif(demand.data.h2o, seed = 489372)
cvgroup   <- as.numeric(cut(split,quantile(split,probs = seq(0, 1, 1/K)),include.lowest = TRUE)) + 1
j=1
ii  <- cvgroup == j
nii <- cvgroup != j
if(K==1){
  ii  <- cvgroup == j
  nii <- cvgroup == j
}
datause <- demand.data.h2o[nii,]
dataout <- demand.data.h2o[ii,]


# ===================================================================
# Inputs
datause=datause
dataout=dataout
var.y=Y[1]
var.x=X
split.ratio= 0.8
num.folds=10
glm_params= list(seed = c(38278),
                 tweedie_variance_power = c(0),
                 tweedie_link_power = c(1), 
                 alpha = c(0,0.25,0.5, 0.75, 1),
                 lambda = c(0,0.1,0.5,1,2,5,10,50, 100),
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