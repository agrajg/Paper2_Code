# cat('Pre-requisites ... ', '\n')
# source(file = "01_01_Reading_CSV_and_Saving_Data.R")

cat('R Setup', '\n')
cat('-------', '\n')
{
  cat('Clearing up workspace ...', '\n')
  rm(list = ls())
  source(file = "00_00_Preamble.R")

  cat('Loading packages needed in this program ...', '\n')
  packages<-c("ggplot2", "foreach", "doParallel", "h2o", "stringr", "stringi", "gmm", "tidyverse", "tidyr", "dplyr")
  # remove.packages(packages, lib = .libPaths())
  # install.packages(packages, dependencies = TRUE)
  check.packages(packages)
  
  cat('H2O port ...', '\n')
  n.threads = 32
  port.num = 11112
  max.mem = '500G'
  force.DL = TRUE
  
  cat('R Setup', '\n')
  cat('=======', '\n')
}

cat('Data cleanup and preparation.', '\n')
cat('-----------------------------', '\n')
{
  source(file = "DML_Function.R") # Here we define all the functions used in this program
  source(file = "Loading_the_data_file.R") # Loading the data file 
  source(file = "Take_a_sample.R")     # Take a sample     # *** (this part need to be commented out) ***
  source(file = "Data_setup.R") # Prepare the data
  source(file = "Creating_relevant_variables.R")   # Creating relevant variables
  
  cat('Data cleanup and preparation.', '\n')
  cat('=============================', '\n')
}

cat('MAIN PROGRAM', '\n')
cat('------------', '\n')
{
  cat('Take all parmeters as inputs.','\n')
  cat('--------------------------------','\n')
  {
    file.num = "50_00"                           # *** Change this in the final program
    method = "GLM_No_Lambda_Search"              # *** Change this in the final program
    is.test = "Test"                             # *** Change this in the final program
  
    cat('For ML algorithms ...', '\n')
    cv.num.folds=0                                # *** Change this in the final program
    grid = "alpha_lambda"                         # *** Change this in the final program
    grid_GLM = list(alpha = c(1), lambda = c(0.001, 0.01))   # *** Change this in the final program
    
    cat('For DML algorithm ...', '\n')            
    times.split.dml = 2                           # *** Change this in the final program
    crossfit.K = 2                                # *** Change this in the final program
    cat('Take all parmeters as inputs.','\n')
    cat('================================','\n')
  }
  
  cat('Defining variables.', '\n')
  cat('-------------------', '\n')
  {
    cat('Y ...','\n')
    Y <- c("qdemand")
    
    cat('D ...','\n')
    D <- c("lprice_per_person")
    
    cat('Z ...','\n')
    Z1 <- c("prod_week1", "prod_week2", "prod_week3", "prod_week4", "prod_week5", 
            "prod_week1_contcap", "prod_week2_contcap", "prod_week3_contcap", "prod_week4_contcap", "prod_week5_contcap")
    
    Z <- setdiff(str_subset(final.df.h2o %>% h2o.colnames(), "prod_week") , c("proddum",Z1))
    
    cat('X ...','\n')
    X1 <- c("qdemand_l1","qdemand_l2" ,"qdemand_l3", "listingtype", "bedrooms", "bathrooms", "nbhd", "latitude","longitude", "p_age", "h_age" ,"p_dayshosting" ,
            "h_dayshosting" , "p_daysbooked" ,"h_daysbooked", "p_guestcount", "h_guestcount","propertyid", "date", "year", "week", "dayOfWeek")
    X2 <- str_subset(final.df.h2o %>% h2o.colnames(), "_textvar")
    X <- c(X1,X2)
    
    cat('Defining variables.', '\n')
    cat('===================', '\n')
  }
  
  cat('Subset Data (if needed)', '\n')
  cat('-----------------------', '\n')
  {
    X <- X[1:30]                     # *** Remove this in the final program***
    Z <- Z[1:4]                      # *** Remove this in the final program***
    Z <- Z1                          # Alternate specification

    cat('Subset Data (if needed).', '\n')
    cat('========================', '\n')
  }
  
  cat('Print all the inputs for DML Main program. ','\n')
  cat('-------------------------------------------','\n')
  {
    cat('General : ', '\n')
    cat('----------', '\n')
    cat('file.num = ', file.num ,'\n') 
    cat('method = ', method ,'\n')
    cat('is.test = ', is.test,'\n')
    cat('==========', '\n')
    
    cat('For ML algorithms : ', '\n')
    cat('--------------------', '\n')
    cat('cv.num.folds = ', cv.num.folds , '\n')
    cat('grid = ', grid , '\n')
    cat('gridGLM = ','\n')
    print(grid_GLM)
    cat('====================', '\n')
    
    cat('For DML algorithm : ', '\n') 
    cat('--------------------', '\n')
    cat('times.split.dml = ', times.split.dml ,'\n')
    cat('crossfit.K = ', crossfit.K ,'\n')
    cat('====================', '\n')
    
    cat('Final Set of Variables for Model Selection and DML : ', '\n')
    cat('-----------------------------------------------------', '\n')
    cat('Y : ', '\n')
    cat('-----------------------------------------------------', '\n')
    print(Y)
    cat('=====================================================', '\n')
    
    cat('D : ', '\n')
    cat('-----------------------------------------------------', '\n')
    print(D)
    cat('=====================================================', '\n')
    
    cat('Z : ', '\n')
    cat('-----------------------------------------------------', '\n')
    print(Z)
    cat('=====================================================', '\n')
    
    cat('X : ', '\n')
    cat('-----------------------------------------------------', '\n')
    print(X)
    cat('=====================================================', '\n')
    
    cat('Print all the inputs for DML Main program. ','\n')
    cat('===========================================','\n')
  }

  cat('Apply DML with best GLM without lambda search.','\n')
  cat('----------------------------------------------','\n')
  {
    cat('Defining Predictor and Target Variables','\n') 
    cat('-------------------------------','\n') 
    var.x = X                         
    ML_outcome.list  <- c(Y,D,Z)
    
  }
  
}



  cat('Initializing results lists etc.','\n') 
  cat('-------------------------------','\n') 
  {
    stats.name <- c("beta", "var")
    results.list.names <- apply(expand.grid(stats.name, D), 1, paste, collapse=".")
    cat('Initializing DML 1 list : results.DML1.','\n')
    results.DML1 <- vector("list", length = length(results.list.names))
    names(results.DML1) <- results.list.names
    cat('Initializing DML 2 list : results.DML2.','\n')
    results.DML2 <- vector("list", length = length(results.list.names))
    names(results.DML2) <- results.list.names
  }
  cat('===============================','\n') 
  
  cat('Repeating Model selection, DML 1 and DML 2 ', times.split.dml , 'times.','\n')
  cat('----------------------------------------------------------------------', '\n')
  for(i in 1:times.split.dml) {
    i=1
    cat('Split the data into model selection data and DML data', '\n')
    
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
  cat('======================================================================', '\n')
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
  cat('MAIN PROGRAM', '\n')
  cat('============', '\n')
}
