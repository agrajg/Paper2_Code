cat('* Procedure for DL, Iteration = ', i , '\n')
{
  cat('* Initilizing cross fit predicted outcome to pool for DML ... ', '\n') 
  {
    fold.pool  <- final_df_h2o$fold_numbers %>% h2o.asnumeric() %>% as.matrix()
    resYX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
    resDX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
    resZX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
  }
  
  cat('* Estimate nuisance function with DL, Iteration = ', i , '\n')
  {
    for(y in Y){
      # y=Y[1]
      cat('* y variable is : ' , y , '\n')
      cat('* Check to see if ', y , 'is a BINARY variable ... ',  '\n')
      if(h2o.isfactor(final_df_h2o[,y])){
        cat('*', y , 'is a BINARY variable.' , '\n')
      } else {
        cat('*', y , 'is a CONTINIOUS variable.' , '\n')
      }
      
      cat('* Run DL for y : ', y, ', predict and combine residuals','\n')
      {
        cat('* Run DL , Iteration = ', i , '\n')
        {
          seed = sample(1:999999, 1, replace=TRUE)
          resyX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
          DL_fit <- h2o.deeplearning(model_id = paste("DL", y, "iequals",as.character(i), sep = "_"), 
                                     hidden = as.numeric(strsplit(as.character(Results_Grid$DL_hidden[which(Results_Grid$DML_iteration == i
                                                                                                            & Results_Grid$target_name == y
                                                                                                            & Results_Grid$method_name == "DL")]),
                                                                  split = "_")[[1]]),
                                     activation = as.character(Results_Grid$DL_activation[which(Results_Grid$DML_iteration == i
                                                                                                & Results_Grid$target_name == y
                                                                                                & Results_Grid$method_name == "DL")]), 
                                     l1 = Results_Grid$DL_l1[which(Results_Grid$DML_iteration == i
                                                                   & Results_Grid$target_name == y
                                                                   & Results_Grid$method_name == "DL")], 
                                     l2 = Results_Grid$DL_l2[which(Results_Grid$DML_iteration == i
                                                                   & Results_Grid$target_name == y
                                                                   & Results_Grid$method_name == "DL")], 
                                     x = predictor_list, 
                                     y = y, 
                                     training_frame = final_df_h2o, 
                                     fold_column="fold_numbers",
                                     keep_cross_validation_predictions = TRUE,
                                     seed = seed, 
                                     epochs = epochs_param_DL, 
                                     distribution = distribution_param_DL, 
                                     score_interval = score_interval_param_DL,
                                     score_training_samples = score_training_samples_param_DL, 
                                     score_validation_samples = score_validation_samples_param_DL,
                                     score_duty_cycle = score_duty_cycle_param_DL, 
                                     classification_stop = classification_stop_param_DL,
                                     regression_stop = regression_stop_param_DL,
                                     stopping_rounds = stopping_rounds_param_DL,
                                     stopping_metric = stopping_metric_param_DL,
                                     stopping_tolerance = stopping_tolerance_param_DL, 
                                     max_runtime_secs = max_runtime_secs_param_DL, 
                                     sparse = sparse_param_DL
          )
          # cat('* Save the model ... ', '\n')
          # h2o.saveModel(object=DL_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)  
        }
        
        cat('* Storing DL details ... ', '\n')
        {
          Results_ML_Performance$var_label <- c(Results_ML_Performance$var_label, "Y")
          Results_ML_Performance$DML_iteration <- c(Results_ML_Performance$DML_iteration, i)
          Results_ML_Performance$DML_fold_seed <- c(Results_ML_Performance$DML_fold_seed, seed_h2o)
          Results_ML_Performance$target_name <- c(Results_ML_Performance$target_name, y)
          Results_ML_Performance$method_name <- c(Results_ML_Performance$method_name, "DL")
          Results_ML_Performance$train_MSE <- c(Results_ML_Performance$train_MSE, h2o.performance(DL_fit, train = TRUE, xval = FALSE)@metrics$MSE)
          Results_ML_Performance$xval_MSE <- c(Results_ML_Performance$xval_MSE, h2o.performance(DL_fit, train = FALSE, xval = TRUE)@metrics$MSE)
          Results_ML_Performance$parameters <- c(Results_ML_Performance$parameters, paste("hidden = ", paste(as.character(DL_fit@allparameters$hidden), collapse  = "-"), ",",
                                                                            "activation = ", DL_fit@allparameters$activation, ",",
                                                                            "l1 = ", DL_fit@allparameters$l1, ",",
                                                                            "l2 = ", DL_fit@allparameters$l2,
                                                                            sep = " "))
          Results_ML_Performance$seed <- c(Results_ML_Performance$seed, DL_fit@allparameters$seed)
        }
        
        cat('* Create Vector of residuls of y = ', y ,'\n')
        {
          if(h2o.isfactor(final_df_h2o[,y])){
            resyX.pool <- (h2o.asnumeric(final_df_h2o[,y]) - h2o.getFrame(DL_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
            cat('*', y, 'is a BINARY variable.' , '\n')
          } else {
            resyX.pool <- (h2o.asnumeric(final_df_h2o[,y]) - h2o.getFrame(DL_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
            cat('*', y, 'is a CONTINIOUS variable.' , '\n')
          }
          cat('* Cbind with residuals of Y ... ', '\n')
          resYX.pool <- cbind(resYX.pool, resyX.pool)
        }
        
        cat('* Remove objects created by DL on y : ', y ,'\n')
        {
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("DL", y, "iequals",as.character(i), sep = "_")));
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
      
      cat('* Run DL for d : ', d, ', predict and combine residuals','\n')
      {
        cat('* Run DL , Iteration = ', i , '\n')
        {
          seed = sample(1:999999, 1, replace=TRUE)
          resdX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
          DL_fit <- h2o.deeplearning(model_id = paste("DL", d, "iequals",as.character(i), sep = "_"), 
                                     hidden = as.numeric(strsplit(as.character(Results_Grid$DL_hidden[which(Results_Grid$DML_iteration == i
                                                                                                            & Results_Grid$target_name == d
                                                                                                            & Results_Grid$method_name == "DL")]),
                                                                  split = "_")[[1]]),
                                     activation = as.character(Results_Grid$DL_activation[which(Results_Grid$DML_iteration == i
                                                                                                & Results_Grid$target_name == d
                                                                                                & Results_Grid$method_name == "DL")]), 
                                     l1 = Results_Grid$DL_l1[which(Results_Grid$DML_iteration == i
                                                                   & Results_Grid$target_name == d
                                                                   & Results_Grid$method_name == "DL")], 
                                     l2 = Results_Grid$DL_l2[which(Results_Grid$DML_iteration == i
                                                                   & Results_Grid$target_name == d
                                                                   & Results_Grid$method_name == "DL")], 
                                     x = predictor_list, 
                                     y = d, 
                                     training_frame = final_df_h2o, 
                                     fold_column="fold_numbers",
                                     keep_cross_validation_predictions = TRUE,
                                     seed = seed, 
                                     epochs = epochs_param_DL, 
                                     distribution = distribution_param_DL, 
                                     score_interval = score_interval_param_DL,
                                     score_training_samples = score_training_samples_param_DL, 
                                     score_validation_samples = score_validation_samples_param_DL,
                                     score_duty_cycle = score_duty_cycle_param_DL, 
                                     classification_stop = classification_stop_param_DL,
                                     regression_stop = regression_stop_param_DL,
                                     stopping_rounds = stopping_rounds_param_DL,
                                     stopping_metric = stopping_metric_param_DL,
                                     stopping_tolerance = stopping_tolerance_param_DL, 
                                     max_runtime_secs = max_runtime_secs_param_DL, 
                                     sparse = sparse_param_DL
          )
          # cat('* Save the model ... ', '\n')
          # h2o.saveModel(object=DL_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)  
        }
        
        cat('* Storing DL details ... ', '\n')
        {
          Results_ML_Performance$var_label <- c(Results_ML_Performance$var_label, "D")
          Results_ML_Performance$DML_iteration <- c(Results_ML_Performance$DML_iteration, i)
          Results_ML_Performance$DML_fold_seed <- c(Results_ML_Performance$DML_fold_seed, seed_h2o)
          Results_ML_Performance$target_name <- c(Results_ML_Performance$target_name, d)
          Results_ML_Performance$method_name <- c(Results_ML_Performance$method_name, "DL")
          Results_ML_Performance$train_MSE <- c(Results_ML_Performance$train_MSE, h2o.performance(DL_fit, train = TRUE, xval = FALSE)@metrics$MSE)
          Results_ML_Performance$xval_MSE <- c(Results_ML_Performance$xval_MSE, h2o.performance(DL_fit, train = FALSE, xval = TRUE)@metrics$MSE)
          Results_ML_Performance$parameters <- c(Results_ML_Performance$parameters, paste("hidden = ", paste(as.character(DL_fit@allparameters$hidden), collapse  = "-"), ",",
                                                                            "activation = ", DL_fit@allparameters$activation, ",",
                                                                            "l1 = ", DL_fit@allparameters$l1, ",",
                                                                            "l2 = ", DL_fit@allparameters$l2,
                                                                            sep = " "))
          Results_ML_Performance$seed <- c(Results_ML_Performance$seed, DL_fit@allparameters$seed)
        }
        
        cat('* Create Vector of residuls of d = ', d ,'\n')
        {
          if(h2o.isfactor(final_df_h2o[,d])){
            resdX.pool <- (h2o.asnumeric(final_df_h2o[,d]) - h2o.getFrame(DL_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
            cat('*', d, 'is a BINARY variable.' , '\n')
          } else {
            resdX.pool <- (h2o.asnumeric(final_df_h2o[,d]) - h2o.getFrame(DL_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
            cat('*', d, 'is a CONTINIOUS variable.' , '\n')
          }
          cat('* Cbind with residuals of D ... ', '\n')
          resDX.pool <- cbind(resDX.pool, resdX.pool)
        }
        
        cat('* Remove objects created by DL on d : ', d ,'\n')
        {
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("DL", d, "iequals",as.character(i), sep = "_")));
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
      
      cat('* Run DL for z : ', z, ', predict and combine residuals','\n')
      {
        cat('* Run DL , Iteration = ', i , '\n')
        {
          seed = sample(1:999999, 1, replace=TRUE)
          reszX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
          DL_fit <- h2o.deeplearning(model_id = paste("DL", z, "iequals",as.character(i), sep = "_"), 
                                     hidden = as.numeric(strsplit(as.character(Results_Grid$DL_hidden[which(Results_Grid$DML_iteration == i
                                                                                                            & Results_Grid$target_name == z
                                                                                                            & Results_Grid$method_name == "DL")]),
                                                                  split = "_")[[1]]),
                                     activation = as.character(Results_Grid$DL_activation[which(Results_Grid$DML_iteration == i
                                                                                                & Results_Grid$target_name == z
                                                                                                & Results_Grid$method_name == "DL")]), 
                                     l1 = Results_Grid$DL_l1[which(Results_Grid$DML_iteration == i
                                                                   & Results_Grid$target_name == z
                                                                   & Results_Grid$method_name == "DL")], 
                                     l2 = Results_Grid$DL_l2[which(Results_Grid$DML_iteration == i
                                                                   & Results_Grid$target_name == z
                                                                   & Results_Grid$method_name == "DL")], 
                                     x = predictor_list, 
                                     y = z, 
                                     training_frame = final_df_h2o, 
                                     fold_column="fold_numbers",
                                     keep_cross_validation_predictions = TRUE,
                                     seed = seed, 
                                     epochs = epochs_param_DL, 
                                     distribution = distribution_param_DL, 
                                     score_interval = score_interval_param_DL,
                                     score_training_samples = score_training_samples_param_DL, 
                                     score_validation_samples = score_validation_samples_param_DL,
                                     score_duty_cycle = score_duty_cycle_param_DL, 
                                     classification_stop = classification_stop_param_DL,
                                     regression_stop = regression_stop_param_DL,
                                     stopping_rounds = stopping_rounds_param_DL,
                                     stopping_metric = stopping_metric_param_DL,
                                     stopping_tolerance = stopping_tolerance_param_DL, 
                                     max_runtime_secs = max_runtime_secs_param_DL, 
                                     sparse = sparse_param_DL
          )
          # cat('* Save the model ... ', '\n')
          # h2o.saveModel(object=DL_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)  
        }
        
        cat('* Storing DL details ... ', '\n')
        {
          Results_ML_Performance$var_label <- c(Results_ML_Performance$var_label, "Z")
          Results_ML_Performance$DML_iteration <- c(Results_ML_Performance$DML_iteration, i)
          Results_ML_Performance$DML_fold_seed <- c(Results_ML_Performance$DML_fold_seed, seed_h2o)
          Results_ML_Performance$target_name <- c(Results_ML_Performance$target_name, z)
          Results_ML_Performance$method_name <- c(Results_ML_Performance$method_name, "DL")
          Results_ML_Performance$train_MSE <- c(Results_ML_Performance$train_MSE, h2o.performance(DL_fit, train = TRUE, xval = FALSE)@metrics$MSE)
          Results_ML_Performance$xval_MSE <- c(Results_ML_Performance$xval_MSE, h2o.performance(DL_fit, train = FALSE, xval = TRUE)@metrics$MSE)
          Results_ML_Performance$parameters <- c(Results_ML_Performance$parameters, paste("hidden = ", paste(as.character(DL_fit@allparameters$hidden), collapse  = "-"), ",",
                                                                            "activation = ", DL_fit@allparameters$activation, ",",
                                                                            "l1 = ", DL_fit@allparameters$l1, ",",
                                                                            "l2 = ", DL_fit@allparameters$l2,
                                                                            sep = " "))
          Results_ML_Performance$seed <- c(Results_ML_Performance$seed, DL_fit@allparameters$seed)
        }
        
        cat('* Create Vector of residuls of z = ', z ,'\n')
        {
          if(h2o.isfactor(final_df_h2o[,z])){
            reszX.pool <- (h2o.asnumeric(final_df_h2o[,z]) - h2o.getFrame(DL_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
            cat('*', z, 'is a BINARY variable.' , '\n')
          } else {
            reszX.pool <- (h2o.asnumeric(final_df_h2o[,z]) - h2o.getFrame(DL_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
            cat('*', z, 'is a CONTINIOUS variable.' , '\n')
          }
          cat('* Cbind with residuals of Z ... ', '\n')
          resZX.pool <- cbind(resZX.pool, reszX.pool)
        }
        
        cat('* Remove objects created by DL on z : ', z ,'\n')
        {
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("DL", z, "iequals",as.character(i), sep = "_")));
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
      Results_DML$method_name <- c(Results_DML$method_name, "DL")
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
      Results_DML$method_name <- c(Results_DML$method_name, "DL")
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