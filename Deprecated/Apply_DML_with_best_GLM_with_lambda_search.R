# ***********************************************
# sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
cat('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>', '\n')
cat('Begin Running file : ', "Apply_DML_with_best_GLM_with_lambda_search.R", " ...", '\n')
# sink()
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
      model.parameters <- best_glm_grid_lambda_search_df %>% filter(outcome == y)
      fam = "gaussian"
      if(model.parameters$outcome_type == "bin"){ fam = "binomial" }
      resyx <- vector()
      yx <- glm_lambda_search(var.x = var.x, 
                              var.y = y, 
                              datause = datause, 
                              dataout = dataout, 
                              n.folds = cv.num.folds, 
                              alpha = model.parameters$alpha, 
                              family = fam, 
                              lambda.search = TRUE)
      resyx <- yx[["resout"]]
      resYX <- cbind(resYX, resyx)
    }
    
    
    resDX <- vector()
    for(d in D){
      model.parameters <- best_glm_grid_lambda_search_df %>% filter(outcome == d)
      fam = "gaussian"
      if(model.parameters$outcome_type == "bin"){ fam = "binomial" }
      resdx <- vector()
      dx <- glm_lambda_search(var.x = var.x, 
                                var.y = d, 
                                datause = datause, 
                                dataout = dataout, 
                                n.folds = cv.num.folds, 
                                alpha = model.parameters$alpha, 
                                family = fam, 
                                lambda.search = TRUE)
      resdx <- dx[["resout"]]
      resDX <- cbind(resDX, resdx)
    }
    
    resZX <- vector()
    for(z in Z){          # *** Keep this in the final program ***
    # for(z in Z[1:4]){       # *** Remove this in the final program ***
      model.parameters <- best_glm_grid_lambda_search_df %>% filter(outcome == z)
      fam = "gaussian"
      if(model.parameters$outcome_type == "bin"){ fam = "binomial" }
      reszx <- vector()
      zx <- glm_lambda_search(var.x = var.x, 
                                var.y = z, 
                                datause = datause, 
                                dataout = dataout, 
                                n.folds = cv.num.folds, 
                                alpha = model.parameters$alpha, 
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
  gmm.pool <- tsls(resYX~resDX,~resZX)
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
# sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
cat(' ... ', '\n')
cat('End Running file : ', "Apply_DML_with_best_GLM_with_lambda_search.R", '\n')
cat('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<', '\n')
# sink()  # *** Keep in the final run ***
# ***********************************************
