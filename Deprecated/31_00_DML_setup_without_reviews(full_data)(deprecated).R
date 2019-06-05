# Clearing up workspace
# ---------------------------------------------------------------------------------------------------------------------
rm(list = ls())
source(file = "00_00_Preamble.R")

# Packages needed in this program
# ---------------------------------------------------------------------------------------------------------------------
packages<-c("ggplot2", "foreach", "doParallel", "dplyr", "caret", "h2o", "stringr", "gmm")
check.packages(packages)
# =====================================================================================================================


# Loading the data file 
# ---------------------------------------------------------------------------------------------------------------------
ptm.loadRdata <- proc.time()
load(file = paste(project.path,"Output/TEMP/","11_00_Demand_Regression_data.RData", sep=""))
proc.time() - ptm.loadRdata
# =====================================================================================================================

# # Take a sample (this part need to be commented out)
# # ---------------------------------------------------------------------------------------------------------------------
# # demand.data.2 <- demand.data
# demand.data <- sample_frac(demand.data, size = 0.01)
# # =====================================================================================================================

# Setting things in H2O environment and loading the entire data on to H2O environment.
# This may take about 20 minutes
# ---------------------------------------------------------------------------------------------------------------------
h2o.init(port = 11111, nthreads = -1, max_mem_size = '1000G')
h2o.removeAll() ## clean slate - just in case the cluster was already running
ptm.loadH2Odata <- proc.time()
demand.data.h2o <- as.h2o(demand.data)
proc.time() - ptm.loadH2Odata
# =====================================================================================================================


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
head(demand.data.h2o)
class(demand.data.h2o)
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

# Main Algorithhm
# ---------------------------------------------------------------------------------------------------------------------
# To make results robust to partitioning.
# Number of times we want to repeat DML. 
times.split = 1

# Kfolds for crossfitting
nfold = 2

# Initializing results lists etc.
stats.name <- c("beta", "var")
results.list.names <- apply(expand.grid(stats.name, D), 1, paste, collapse=".")
results.DML1 <- vector("list", length = length(results.list.names))
names(results.DML1) <- results.list.names
results.DML2 <- vector("list", length = length(results.list.names))
names(results.DML2) <- results.list.names

# Defining Functions 
# ---------------------------------------------------------------------------------------------------------------------
source(paste(project.path,'Code/','DML_Function.R', sep = ""), echo=TRUE)
# =====================================================================================================================

# Starting the procedure
# ---------------------------------------------------------------------------------------------------------------------
# cl <- makeCluster(20)
# registerDoParallel(cl)
# # r <- foreach(k = 1:times.split, .combine='c', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %do% { 
# r <- foreach(k = 1:times.split, .combine='c', .inorder=FALSE) %do% { 
for(k in 1:times.split){
  # defining inputs of DML function 
  data <- demand.data.h2o
  Y <- Y
  D <- D
  Z <- Z
  X <- X 
  K <- nfold
  
  
  # DML function would begin here:  
  # Procedure
  split     <- h2o.runif(data, seed = 489372)
  cvgroup   <- as.numeric(cut(split,quantile(split,probs = seq(0, 1, 1/K)),include.lowest = TRUE)) + 1
  
  # Initilizing output DS : this is local list that will store gmm results for each cross fit predicted data.
  results.gmm <-vector("list", length = length(results.list.names))
  names(results.gmm) <- results.list.names
  results.gmm[results.list.names] <- 0  
  # Initilizing cross fit predicted outcome to pool for DML2 
  resYX.pool <- vector()
  resDX.pool <- vector()
  resZX.pool <- vector()
  
  # Begin cross fitting
  for(j in 1:K) {
    # j=1
    ii  <- cvgroup == j
    nii <- cvgroup != j
    if(K==1){
      ii  <- cvgroup == j
      nii <- cvgroup == j
    }
    datause <- data[nii,]
    dataout <- data[ii,]
    cat("H2O objects : ", "\n")
    print(h2o.ls())
    
    # Inputs
    glm_params1 <- list(seed = c(38278),
                        tweedie_variance_power = c(0),
                        tweedie_link_power = c(1), 
                        alpha = c(0.5),
                        lambda = c(0.5),
                        missing_values_handling = c("Skip")) # c("MeanImputation", "Skip")
    cat("H2O objects : ", "\n")
    print(h2o.ls())
    
    resYX <- vector()
    for(y in Y){
      resyx <- vector()
      yx <- glm_ml(datause=datause, dataout=dataout, var.y=y, var.x=X, split.ratio= 0.8, num.folds=10, glm_params=glm_params1)
      resyx <- yx[["resout"]]
      resYX <- cbind(resYX, resyx)
    }
    h2o.ls()
    
    resDX <- vector()
    for(d in D){
      resdx <- vector()
      dx <- glm_ml(datause=datause, dataout=dataout, var.y=d, var.x=X, split.ratio= 0.8, num.folds=10, glm_params=glm_params1)
      resdx <- dx[["resout"]]
      resDX <- cbind(resDX, resdx)
    }
    h2o.ls()
    
    resZX <- vector()
    for(z in Z){
      reszx <- vector()
      zx <- glm_ml(datause=datause, dataout=dataout, var.y=z, var.x=X, split.ratio= 0.8, num.folds=10, glm_params=glm_params1)
      reszx <- zx[["resout"]]
      resZX <- cbind(resZX, reszx)
    }
    h2o.ls()
    
    gmm <- tsls(resYX~resDX,~resZX)
    
    for(lc in 1:length(D)){
      results.gmm[[(2*lc-1)]] <- results.gmm[[(2*lc-1)]] + (gmm$coefficients[[lc+1]]/K) 
      results.gmm[[2*lc]] <- results.gmm[[2*lc]] + (gmm$vcov[lc+1,lc+1]/(K^2))
    }
    h2o.ls()
    
    resYX.pool <- rbind(resYX.pool, resYX)
    resDX.pool <- rbind(resDX.pool, resDX)
    resZX.pool <- rbind(resZX.pool, resZX)
    h2o.ls()
  }
  
  
  # Save results for each of num.splits
  # remember later save results for each ML method.
  gmm.pool <- tsls(resYX.pool~resDX.pool,~resZX.pool)
  for(lc in 1:length(D)){
    results.DML1[[(2*lc-1)]] <- c(results.DML1[[(2*lc-1)]], results.gmm[[(2*lc-1)]])
    results.DML1[[(2*lc)]] <- c(results.DML1[[(2*lc)]], sqrt(results.gmm[[2*lc]]))
    results.DML2[[(2*lc-1)]] <- c(results.DML2[[(2*lc-1)]], gmm.pool$coefficients[[lc+1]])
    results.DML2[[2*lc]] <- c(results.DML2[[2*lc]], sqrt(gmm.pool$vcov[lc+1,lc+1]))
  }
}
h2o.ls()
cat("DML 1 Results", "\n")
cat("---------------------------------------------------------------------------------------------------------------------------------------", "\n")
results.DML1
cat("=======================================================================================================================================", "\n")
cat("---------------------------------------------------------------------------------------------------------------------------------------", "\n")
sapply(results.DML1, function(x) median(x))
cat("=======================================================================================================================================", "\n")

cat("DML 2 Results", "\n")
cat("---------------------------------------------------------------------------------------------------------------------------------------", "\n")
results.DML2
cat("=======================================================================================================================================", "\n")
cat("---------------------------------------------------------------------------------------------------------------------------------------", "\n")
sapply(results.DML2, function(x) median(x))
cat("=======================================================================================================================================", "\n")


# h2o.shutdown(prompt = FALSE)


