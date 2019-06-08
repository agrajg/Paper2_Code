# Clearing up workspace
# ---------------------------------------------------------------------------------------------------------------------
{
rm(list = ls())
source(file = "00_00_Preamble.R")
# =====================================================================================================================
# Packages needed in this program
# ---------------------------------------------------------------------------------------------------------------------
packages<-c("ggplot2", "foreach", "doParallel", "h2o", "stringr", "stringi", "gmm", "tidyverse", "tidyr", "dplyr")
# remove.packages(packages, lib = .libPaths())
install.packages(packages, dependencies = TRUE)
check.packages(packages)
# =====================================================================================================================
}
# 
# for h2o cluster
port.num = 11110
max.mem = '1000G'
source(file = "DML_Function.R") # Here we define all the functions used in this program
source(file = "Loading_the_data_file.R") # Loading the data file 
source(file = "Take_a_sample.R")     # Take a sample     # *** (this part need to be commented out) ***
source(file = "Data_setup.R") # Prepare the data
source(file = "Creating_relevant_variables.R")   # Creating relevant variables


# =====================================================================================================================
# MAIN PROGRAM
# =====================================================================================================================
{
# Take all parmeters as inputs
file.num = "61_00"
method = "GLM_No_lambda_Search"
is.test = "test"
# Defining parameters 
# ---------------------------------------------------------------------------------------------------------------------
# for ML algorithms
cv.num.folds=0        # *** Change this in the final program
times.split.grid = 1         # *** Change this in the final program ***
grid = "alpha_1_lambda_pt001"
alpha = 1
lambda = 0.01  
# For DML
times.split.dml = 2
crossfit.K = 2
output.filename = paste(file.num,"DML_Output",is.test, method , grid, "CVfold", as.character(cv.num.folds),"grsp",as.character(times.split.grid),
                        "dmlsp", as.character(times.split.dml),"dmlK", as.character(crossfit.K),sep = "_")
cat('Output sent to : ', '\n')
print(output.filename)
# sink(file = paste(project.path,"Output/Final/",output.filename,".txt", sep=""), append = FALSE)    # *** Keep in the final run ***
cat('This file : ', '\n')
print(output.filename)

# Defining input output and other parameters
# ---------------------------------------------------------------------------------------------------------------------
# Y
Y <- c("qdemand")
# D
D <- c("lprice_per_person")
# Z
Z1 <- c("prod_week1", "prod_week2", "prod_week3", "prod_week4", "prod_week5", 
        "prod_week1_contcap", "prod_week2_contcap", "prod_week3_contcap", "prod_week4_contcap", "prod_week5_contcap")

Z <- setdiff(str_subset(final.df.h2o %>% h2o.colnames(), "prod_week") , c("proddum",Z1))
# X
X1 <- c("qdemand_l1","qdemand_l2" ,"qdemand_l3", "listingtype", "bedrooms", "bathrooms", "nbhd", "latitude","longitude", "p_age", "h_age" ,"p_dayshosting" ,
        "h_dayshosting" , "p_daysbooked" ,"h_daysbooked", "p_guestcount", "h_guestcount","propertyid", "date", "year", "week", "dayOfWeek")
X2 <- str_subset(final.df.h2o %>% h2o.colnames(), "_textvar")
X <- c(X1,X2)

# Subset if needed
# ---------------------------------------------------------------------------------------------------------------------
# var.x = X[1:30]                     # *** Remove this in the final program***
# Z <- Z[1:4]                     # *** Remove this in the final program***
Z <- Z1
# ---------------------------------------------------------------------------------------------------------------------
var.x = X                         # ***Keep this in the final program******
ML_outcome.list  <- c(Y,D,Z)
# ---------------------------------------------------------------------------------------------------------------------
# Printing 
cat('cv.num.folds = ', cv.num.folds, '\n')
cat('grid = ', grid, '\n')
cat('alpha = ', alpha, '\n') 
cat('lambda = ', lambda, '\n') 
cat('times.split.dml = ', times.split.dml, '\n')
cat('crossfit.K = ', crossfit.K, '\n')
cat('==============================', '\n')
cat('Y : ', '\n')
cat('------------------------------', '\n')
print(Y)
cat('D : ', '\n')
cat('------------------------------', '\n')
print(D)
cat('Z : ', '\n')
cat('------------------------------', '\n')
print(Z)
cat('X : ', '\n')
cat('------------------------------', '\n')
print(X)
cat('==============================', '\n')
# sink()
}

{
# sink(file = paste(project.path,"Output/Final/",output.filename,".txt", sep=""), append = TRUE)    # *** Keep in the final run ***
# =====================================================================================================================
# Apply DML with best GLM without lambda search
# ---------------------------------------------------------------------------------------------------------------------
# ***********************************************
# # sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
cat('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>', '\n')
cat('Begin Running file : ', "Apply_DML_with_best_GLM_without_lambda_search.R", " ...", '\n')
# # sink()
# ***********************************************

# ---------------------------------------------------------------------------------------------------------------------


# Initializing results lists etc.
stats.name <- c("beta", "var")
results.list.names <- apply(expand.grid(stats.name, D), 1, paste, collapse=".")
results.DML1 <- vector("list", length = length(results.list.names))
names(results.DML1) <- results.list.names
results.DML2 <- vector("list", length = length(results.list.names))
names(results.DML2) <- results.list.names

# Store Final DML 1
results.DML1 <-vector("list", length = length(results.list.names))
names(results.DML1) <- results.list.names
# Store Final DML 2
results.DML2 <-vector("list", length = length(results.list.names))
names(results.DML2) <- results.list.names

# For each time I repeat DML
for(i in 1:times.split.dml) { 
  # i=1 
  # Split the data
  split     <- h2o.runif(final.df.h2o)
  cvgroup   <- as.numeric(cut(split,quantile(split,probs = seq(0, 1, 1/crossfit.K)),include.lowest = TRUE)) + 1
  # Initilizing output DS : this is local list that will store gmm results for each cross fit predicted data.
  # Store DML 1
  results.gmm.dml1 <-vector("list", length = length(results.list.names))
  names(results.gmm.dml1) <- results.list.names
  results.gmm.dml1[results.list.names] <- 0  
  # Store DML 2
  results.gmm.dml2 <-vector("list", length = length(results.list.names))
  names(results.gmm.dml2) <- results.list.names
  
  # Initilizing cross fit predicted outcome to pool for DML2 
  resYX.pool <- vector()
  resDX.pool <- vector()
  resZX.pool <- vector()
  # for each split of the date I perform a cross fit
  for(j in 1:crossfit.K){
    ii  <- cvgroup == j
    nii <- cvgroup != j
    if(crossfit.K==1){
      ii  <- cvgroup == j
      nii <- cvgroup == j
    }
    datause <- final.df.h2o[nii,]
    dataout <- final.df.h2o[ii,]
    # Estimation of nuisance parameters
    resYX <- vector()
    for(y in Y){
      cat('target variable is : ' , '\n')
      print(y)
      fam = "gaussian"
      if(h2o.dim(h2o.unique(final.df.h2o[,y]))[1] == 2){ fam = "binomial" }
      resyx <- vector()
      yx <- glm_nolambda_search(var.x = var.x, 
                                var.y = y, 
                                datause = datause, 
                                dataout = dataout, 
                                n.folds = cv.num.folds, 
                                alpha = alpha, 
                                lambda = lambda,
                                family = fam, 
                                lambda.search = TRUE)
      resyx <- yx[["resout"]]
      resYX <- cbind(resYX, resyx)
    }

    resDX <- vector()
    for(d in D){
      cat('target variable is : ' , '\n')
      print(d)
      fam = "gaussian"
      if(h2o.dim(h2o.unique(final.df.h2o[,d]))[1] == 2){ fam = "binomial" }
      resdx <- vector()
      dx <- glm_nolambda_search(var.x = var.x, 
                                var.y = d, 
                                datause = datause, 
                                dataout = dataout, 
                                n.folds = cv.num.folds, 
                                alpha = alpha,
                                lambda = lambda,
                                family = fam, 
                                lambda.search = TRUE)
      resdx <- dx[["resout"]]
      resDX <- cbind(resDX, resdx)
    }
    
    resZX <- vector()
    for(z in Z){          # *** Keep this in the final program ***
      # for(z in Z[1:4]){       # *** Remove this in the final program ***
      cat('target variable is : ' , '\n')
      print(z)
      fam = "gaussian"
      if(h2o.dim(h2o.unique(final.df.h2o[,z]))[1] == 2){ fam = "binomial" }
      reszx <- vector()
      zx <- glm_nolambda_search(var.x = var.x, 
                                var.y = z, 
                                datause = datause, 
                                dataout = dataout, 
                                n.folds = cv.num.folds, 
                                alpha = alpha, 
                                lambda = lambda,
                                family = fam,
                                lambda.search = TRUE)
      reszx <- zx[["resout"]]
      resZX <- cbind(resZX, reszx)
    }
    
    gmm <- NULL
    gmm <- tsls(resYX~resDX,~resZX)
    for(lc in 1:length(D)){
      results.gmm.dml1[[(2*lc-1)]] <- results.gmm.dml1[[(2*lc-1)]] + (gmm$coefficients[[lc+1]]/crossfit.K) 
      results.gmm.dml1[[2*lc]] <- results.gmm.dml1[[2*lc]] + (gmm$vcov[lc+1,lc+1]/(crossfit.K^2))
    }
    resYX.pool <- rbind(resYX.pool, resYX)
    resDX.pool <- rbind(resDX.pool, resDX)
    resZX.pool <- rbind(resZX.pool, resZX)
  }
  gmm.pool <- NULL
  gmm.pool <- tsls(resYX.pool~resDX.pool,~resZX.pool)
  for(lc in 1:length(D)){
    results.gmm.dml2[[(2*lc-1)]] <-  (gmm$coefficients[[lc+1]]) 
    results.gmm.dml2[[2*lc]] <-  (gmm$vcov[lc+1,lc+1])
  }
  
  # Final results 
  results.DML1 <- rbind(as.data.frame(results.DML1), as.data.frame(results.gmm.dml1))
  results.DML2 <- rbind(as.data.frame(results.DML2), as.data.frame(results.gmm.dml2))
  
} #splits 
cat('DML 1 results')
print(results.DML1)
cat('DML 2 results')
print(results.DML2)

save(results.DML1, 
     file = paste(project.path,"Output/Final/", output.filename ,"DML1_Results.RData", sep="")) # *** Remove this in the final program ***

save(results.DML2, 
     file = paste(project.path,"Output/Final/", output.filename ,"DML2_Results.RData", sep="")) # *** Remove this in the final program ***


# ***********************************************
# # sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
cat(' ... ', '\n')
cat('End Running file : ', "Apply_DML_with_best_GLM_without_lambda_search.R", '\n')
cat('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<', '\n')
# # sink()  # *** Keep in the final run ***
# ***********************************************
# =====================================================================================================================

# sink()                                                                                        # *** Keep in the final run ***
}
