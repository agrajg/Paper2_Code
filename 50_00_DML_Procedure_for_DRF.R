cat('* Procedure for DRF, Iteration = ', i , '\n')
{
  cat('* Initilizing cross fit predicted outcome to pool for DML ... ', '\n') 
  {
    fold.pool  <- final_df_h2o$fold_numbers %>% h2o.asnumeric() %>% as.matrix()
    resYX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
    resDX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
    resZX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
  }
  
  cat('* Estimate nuisance function with DRF, Iteration = ', i , '\n')
  {
    for(y in Y){
      # y=Z[1]
      cat('* y variable is : ' , y , '\n')
      cat('* Check to see if ', y , 'is a BINARY variable ... ',  '\n')
      if(h2o.isfactor(final_df_h2o[,y])){
        cat('*', y , 'is a BINARY variable.' , '\n')
      } else {
        cat('*', y , 'is a CONTINIOUS variable.' , '\n')
      }
      
      cat('* Run DRF for y : ', y, ', predict and combine residuals','\n')
      {
        cat('* Run DRF , Iteration = ', i , '\n')
        {
          seed = sample(1:999999, 1, replace=TRUE)
          resyX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
          DRF_fit <- h2o.randomForest(model_id = paste("DRF", y, "iequals",as.character(i), sep = "_"), 
                                      ntrees = Results_Grid$DRF_ntrees[which(Results_Grid$DML_iteration == i
                                                                             & Results_Grid$target_name == y
                                                                             & Results_Grid$method_name == "DRF")], 
                                      max_depth = Results_Grid$DRF_max_depth[which(Results_Grid$DML_iteration == i 
                                                                                   & Results_Grid$target_name == y 
                                                                                   & Results_Grid$method_name == "DRF")], 
                                      min_rows = Results_Grid$DRF_min_rows[which(Results_Grid$DML_iteration == i 
                                                                                 & Results_Grid$target_name == y 
                                                                                 & Results_Grid$method_name == "DRF")], 
                                      nbins = Results_Grid$DRF_nbins[which(Results_Grid$DML_iteration == i 
                                                                           & Results_Grid$target_name == y 
                                                                           & Results_Grid$method_name == "DRF")], 
                                      x = predictor_list, 
                                      y = y, 
                                      training_frame = final_df_h2o, 
                                      fold_column="fold_numbers",
                                      keep_cross_validation_predictions = TRUE,
                                      seed = seed, 
                                      stopping_rounds = stopping_rounds_param_DRF,
                                      stopping_metric = stopping_metric_param_DRF, 
                                      stopping_tolerance = stopping_tolerance_param_DRF,
                                      min_split_improvement = min_split_improvement_param_DRF
          )
          # cat('* Save the model ... ', '\n')
          # h2o.saveModel(object=DRF_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)  
        }
        
        cat('* Storing DRF details ... ', '\n')
        {
          Results_ML_Performance$var_label <- c(Results_ML_Performance$var_label, "Y")
          Results_ML_Performance$DML_iteration <- c(Results_ML_Performance$DML_iteration, i)
          Results_ML_Performance$DML_fold_seed <- c(Results_ML_Performance$DML_fold_seed, seed_h2o)
          Results_ML_Performance$target_name <- c(Results_ML_Performance$target_name, y)
          Results_ML_Performance$method_name <- c(Results_ML_Performance$method_name, "DRF")
          Results_ML_Performance$train_MSE <- c(Results_ML_Performance$train_MSE, h2o.performance(DRF_fit, train = TRUE, xval = FALSE)@metrics$MSE)
          Results_ML_Performance$xval_MSE <- c(Results_ML_Performance$xval_MSE, h2o.performance(DRF_fit, train = FALSE, xval = TRUE)@metrics$MSE)
          Results_ML_Performance$parameters <- c(Results_ML_Performance$parameters, paste("ntrees = ", DRF_fit@allparameters$ntrees, ",",
                                                                            "max_depth = ", DRF_fit@allparameters$max_depth,  ",",
                                                                            "min_rows = ", DRF_fit@allparameters$min_rows,  ",",
                                                                            "nbins = ", DRF_fit@allparameters$nbins, 
                                                                            sep = " "))
          Results_ML_Performance$seed <- c(Results_ML_Performance$seed, DRF_fit@allparameters$seed)
        }
        
        cat('* Create Vector of residuls of y = ', y ,'\n')
        {
          if(h2o.isfactor(final_df_h2o[,y])){
            resyX.pool <- (h2o.asnumeric(final_df_h2o[,y]) - h2o.getFrame(DRF_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
            cat('*', y, 'is a BINARY variable.' , '\n')
          } else {
            resyX.pool <- (h2o.asnumeric(final_df_h2o[,y]) - h2o.getFrame(DRF_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
            cat('*', y, 'is a CONTINIOUS variable.' , '\n')
          }
          cat('* Cbind with residuals of Y ... ', '\n')
          resYX.pool <- cbind(resYX.pool, resyX.pool)
        }
        
        cat('* Remove objects created by DRF on y : ', y ,'\n')
        {
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("DRF", y, "iequals",as.character(i), sep = "_")));
          h2o.ls()
        }
      }
    }
    
    for(d in D){
      # y=Z[1]
      cat('* d variable is : ' , d , '\n')
      cat('* Check to see if ', d , 'is a BINARY variable ... ',  '\n')
      if(h2o.isfactor(final_df_h2o[,d])){
        cat('*', d , 'is a BINARY variable.' , '\n')
      } else {
        cat('*', d , 'is a CONTINIOUS variable.' , '\n')
      }
      
      cat('* Run DRF for d : ', d, ', predict and combine residuals','\n')
      {
        cat('* Run DRF , Iteration = ', i , '\n')
        {
          seed = sample(1:999999, 1, replace=TRUE)
          resdX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
          DRF_fit <- h2o.randomForest(model_id = paste("DRF", d, "iequals",as.character(i), sep = "_"), 
                                      ntrees = Results_Grid$DRF_ntrees[which(Results_Grid$DML_iteration == i
                                                                             & Results_Grid$target_name == d
                                                                             & Results_Grid$method_name == "DRF")], 
                                      max_depth = Results_Grid$DRF_max_depth[which(Results_Grid$DML_iteration == i 
                                                                                   & Results_Grid$target_name == d 
                                                                                   & Results_Grid$method_name == "DRF")], 
                                      min_rows = Results_Grid$DRF_min_rows[which(Results_Grid$DML_iteration == i 
                                                                                 & Results_Grid$target_name == d 
                                                                                 & Results_Grid$method_name == "DRF")], 
                                      nbins = Results_Grid$DRF_nbins[which(Results_Grid$DML_iteration == i 
                                                                           & Results_Grid$target_name == d 
                                                                           & Results_Grid$method_name == "DRF")], 
                                      x = predictor_list, 
                                      y = d, 
                                      training_frame = final_df_h2o, 
                                      fold_column="fold_numbers",
                                      keep_cross_validation_predictions = TRUE,
                                      seed = seed, 
                                      stopping_rounds = stopping_rounds_param_DRF,
                                      stopping_metric = stopping_metric_param_DRF, 
                                      stopping_tolerance = stopping_tolerance_param_DRF,
                                      min_split_improvement = min_split_improvement_param_DRF
          )
          # cat('* Save the model ... ', '\n')
          # h2o.saveModel(object=DRF_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)  
        }
        
        cat('* Storing DRF details ... ', '\n')
        {
          Results_ML_Performance$var_label <- c(Results_ML_Performance$var_label, "D")
          Results_ML_Performance$DML_iteration <- c(Results_ML_Performance$DML_iteration, i)
          Results_ML_Performance$DML_fold_seed <- c(Results_ML_Performance$DML_fold_seed, seed_h2o)
          Results_ML_Performance$target_name <- c(Results_ML_Performance$target_name, d)
          Results_ML_Performance$method_name <- c(Results_ML_Performance$method_name, "DRF")
          Results_ML_Performance$train_MSE <- c(Results_ML_Performance$train_MSE, h2o.performance(DRF_fit, train = TRUE, xval = FALSE)@metrics$MSE)
          Results_ML_Performance$xval_MSE <- c(Results_ML_Performance$xval_MSE, h2o.performance(DRF_fit, train = FALSE, xval = TRUE)@metrics$MSE)
          Results_ML_Performance$parameters <- c(Results_ML_Performance$parameters, paste("ntrees = ", DRF_fit@allparameters$ntrees, ",",
                                                                            "max_depth = ", DRF_fit@allparameters$max_depth,  ",",
                                                                            "min_rows = ", DRF_fit@allparameters$min_rows,  ",",
                                                                            "nbins = ", DRF_fit@allparameters$nbins, 
                                                                            sep = " "))
          Results_ML_Performance$seed <- c(Results_ML_Performance$seed, DRF_fit@allparameters$seed)
        }
        
        cat('* Create Vector of residuls of d = ', d ,'\n')
        {
          if(h2o.isfactor(final_df_h2o[,d])){
            resdX.pool <- (h2o.asnumeric(final_df_h2o[,d]) - h2o.getFrame(DRF_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
            cat('*', d, 'is a BINARY variable.' , '\n')
          } else {
            resdX.pool <- (h2o.asnumeric(final_df_h2o[,d]) - h2o.getFrame(DRF_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
            cat('*', d, 'is a CONTINIOUS variable.' , '\n')
          }
          cat('* Cbind with residuals of D ... ', '\n')
          resDX.pool <- cbind(resDX.pool, resdX.pool)
        }
        
        cat('* Remove objects created by DRF on d : ', d ,'\n')
        {
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("DRF", d, "iequals",as.character(i), sep = "_")));
          h2o.ls()
        }
      }
    }
    
    for(z in Z){
      # y=Z[1]
      cat('* z variable is : ' , z , '\n')
      cat('* Check to see if ', z , 'is a BINARY variable ... ',  '\n')
      if(h2o.isfactor(final_df_h2o[,z])){
        cat('*', z , 'is a BINARY variable.' , '\n')
      } else {
        cat('*', z , 'is a CONTINIOUS variable.' , '\n')
      }
      
      cat('* Run DRF for z : ', z, ', predict and combine residuals','\n')
      {
        cat('* Run DRF , Iteration = ', i , '\n')
        {
          seed = sample(1:999999, 1, replace=TRUE)
          reszX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
          DRF_fit <- h2o.randomForest(model_id = paste("DRF", z, "iequals",as.character(i), sep = "_"), 
                                      ntrees = Results_Grid$DRF_ntrees[which(Results_Grid$DML_iteration == i
                                                                             & Results_Grid$target_name == z
                                                                             & Results_Grid$method_name == "DRF")], 
                                      max_depth = Results_Grid$DRF_max_depth[which(Results_Grid$DML_iteration == i 
                                                                                   & Results_Grid$target_name == z 
                                                                                   & Results_Grid$method_name == "DRF")], 
                                      min_rows = Results_Grid$DRF_min_rows[which(Results_Grid$DML_iteration == i 
                                                                                 & Results_Grid$target_name == z 
                                                                                 & Results_Grid$method_name == "DRF")], 
                                      nbins = Results_Grid$DRF_nbins[which(Results_Grid$DML_iteration == i 
                                                                           & Results_Grid$target_name == z 
                                                                           & Results_Grid$method_name == "DRF")], 
                                      x = predictor_list, 
                                      y = z, 
                                      training_frame = final_df_h2o, 
                                      fold_column="fold_numbers",
                                      keep_cross_validation_predictions = TRUE,
                                      seed = seed, 
                                      stopping_rounds = stopping_rounds_param_DRF,
                                      stopping_metric = stopping_metric_param_DRF, 
                                      stopping_tolerance = stopping_tolerance_param_DRF,
                                      min_split_improvement = min_split_improvement_param_DRF
          )
          # cat('* Save the model ... ', '\n')
          # h2o.saveModel(object=DRF_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)  
        }
        
        cat('* Storing DRF details ... ', '\n')
        {
          Results_ML_Performance$var_label <- c(Results_ML_Performance$var_label, "Z")
          Results_ML_Performance$DML_iteration <- c(Results_ML_Performance$DML_iteration, i)
          Results_ML_Performance$DML_fold_seed <- c(Results_ML_Performance$DML_fold_seed, seed_h2o)
          Results_ML_Performance$target_name <- c(Results_ML_Performance$target_name, z)
          Results_ML_Performance$method_name <- c(Results_ML_Performance$method_name, "DRF")
          Results_ML_Performance$train_MSE <- c(Results_ML_Performance$train_MSE, h2o.performance(DRF_fit, train = TRUE, xval = FALSE)@metrics$MSE)
          Results_ML_Performance$xval_MSE <- c(Results_ML_Performance$xval_MSE, h2o.performance(DRF_fit, train = FALSE, xval = TRUE)@metrics$MSE)
          Results_ML_Performance$parameters <- c(Results_ML_Performance$parameters, paste("ntrees = ", DRF_fit@allparameters$ntrees, ",",
                                                                            "max_depth = ", DRF_fit@allparameters$max_depth,  ",",
                                                                            "min_rows = ", DRF_fit@allparameters$min_rows,  ",",
                                                                            "nbins = ", DRF_fit@allparameters$nbins, 
                                                                            sep = " "))
          Results_ML_Performance$seed <- c(Results_ML_Performance$seed, DRF_fit@allparameters$seed)
        }
        
        cat('* Create Vector of residuls of z = ', z ,'\n')
        {
          if(h2o.isfactor(final_df_h2o[,z])){
            reszX.pool <- (h2o.asnumeric(final_df_h2o[,z]) - h2o.getFrame(DRF_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
            cat('*', z, 'is a BINARY variable.' , '\n')
          } else {
            reszX.pool <- (h2o.asnumeric(final_df_h2o[,z]) - h2o.getFrame(DRF_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
            cat('*', z, 'is a CONTINIOUS variable.' , '\n')
          }
          cat('* Cbind with residuals of Z ... ', '\n')
          resZX.pool <- cbind(resZX.pool, reszX.pool)
        }
        
        cat('* Remove objects created by DRF on z : ', z ,'\n')
        {
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("DRF", z, "iequals",as.character(i), sep = "_")));
          h2o.ls()
        }
      }
    }
  }
  
  cat('* Perform GMM estimation for DML1, Iteration = ', i , '\n')
  {
    cat('Initializing DML1_combine lists etc. ... ','\n')
    {
      DML1_combine <- vector("list", length = length(apply(expand.grid(c("beta", "var"), D), 1, paste, collapse=".")))
      names(DML1_combine) <- c(apply(expand.grid(c("beta", "var"), D), 1, paste, collapse="."))
      cat('* DML1_combine list : ', '\n')
      DML1_combine[] <- 0
      print(DML1_combine)
    }
    
    for(j in 0:(K.folds - 1)){
      resYX <- resYX.pool[which(fold.pool == j),] %>% as.matrix()
      resDX <- resDX.pool[which(fold.pool == j),] %>% as.matrix()
      resZX <- resZX.pool[which(fold.pool == j),] %>% as.matrix()
      gmm1 <- NULL
      gmm1 <- gmm(resYX~resDX-1,~resZX-1,vcov="iid")
      cat('* DML 1, fold', j , 'results summary', '\n')
      print(gmm1 %>% summary())
      
      for(lc in 1:length(ncol(resDX))){
        DML1_combine[[(-1+2*lc)]] <- DML1_combine[[(-1+2*lc)]] + as.numeric((1/K.folds)  *(as.numeric(gmm1$coefficients[lc])))
        DML1_combine[[(2*lc)]]    <- DML1_combine[[(2*lc)]]    + as.numeric((1/K.folds^2)*(as.numeric(gmm1$vcov[lc,lc])))
      }
    }
    
    cat('* Storing Results of DML 1 ... ', '\n')
    {
      Results_DML$DML_iteration <- c(Results_DML$DML_iteration, i)
      Results_DML$DML_fold_seed <- c(Results_DML$DML_fold_seed, seed_h2o)
      Results_DML$DML_type <- c(Results_DML$DML_type, "DML 1")
      Results_DML$K_folds <- c(Results_DML$K_folds, K.folds)
      Results_DML$method_name <- c(Results_DML$method_name, "DRF")
      for(lc in 1:length(ncol(resDX.pool))){
        Results_DML[[(4+2*lc)]] <- c(Results_DML[[(4+2*lc)]], as.numeric(DML1_combine[[(-1+2*lc)]]))
        Results_DML[[(5+2*lc)]] <- c(Results_DML[[(5+2*lc)]], as.numeric(sqrt(DML1_combine[[(2*lc)]])))
      }
    }
  }
  
  cat('* Perform GMM estimation for DML2, Iteration = ', i , '\n')
  {
    gmm2 <- NULL
    gmm2 <- gmm(resYX.pool~resDX.pool-1,~resZX.pool-1,vcov="iid")
    cat('* DML 2 results summary', '\n')
    print(gmm2 %>% summary())
    cat('* Storing Results of DML 1 ... ', '\n')
    {
      Results_DML$DML_iteration <- c(Results_DML$DML_iteration, i)
      Results_DML$DML_fold_seed <- c(Results_DML$DML_fold_seed, seed_h2o)
      Results_DML$DML_type <- c(Results_DML$DML_type, "DML 2")
      Results_DML$K_folds <- c(Results_DML$K_folds, K.folds)
      Results_DML$method_name <- c(Results_DML$method_name, "DRF")
      for(lc in 1:length(ncol(resDX.pool))){
        Results_DML[[(4+2*lc)]] <- c(Results_DML[[(4+2*lc)]], as.numeric(gmm2$coefficients[lc]))
        Results_DML[[(5+2*lc)]] <- c(Results_DML[[(5+2*lc)]], as.numeric(sqrt(as.numeric(gmm2$vcov[lc,lc]))))
      }
    }
  }
  cat('* Remove objects not needed, Iteration = ', i , '\n')
  {
    rm(gmm1); gc();
    rm(gmm2); gc();
    rm(fold.pool); gc();
    rm(resYX.pool); gc();
    rm(resDX.pool); gc();
    rm(resZX.pool); gc();
    rm(seed); gc();
    rm(resyX.pool); gc();
    rm(resdX.pool); gc();
    rm(reszX.pool); gc();
    rm(DML1_combine); gc();
    rm(resYX); gc();
    rm(resDX); gc();
    rm(resZX); gc();    
  }
}
