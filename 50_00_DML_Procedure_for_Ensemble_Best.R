cat('* Procedure for Ensemble and Best, Iteration = ', i , '\n')
{
  cat('* Initilizing cross fit predicted outcome to pool for DML ... ', '\n') 
  {
    fold.pool  <- final_df_h2o$fold_numbers %>% h2o.asnumeric() %>% as.matrix()
    resYX.pool.Ensemble <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
    resDX.pool.Ensemble <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
    resZX.pool.Ensemble <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
    resYX.pool.Best <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
    resDX.pool.Best <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
    resZX.pool.Best <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
  }
  cat('* Obtain seeds used in each algorithm', '\n')
  {
    Results_ML_Performance_temp <- Results_ML_Performance %>% as.data.frame() 
  }
  cat('* Estimate nuisance function with Ensemble and Best, Iteration = ', i , '\n')
  {
    for(y in Y){
      # y=Y[1]
      cat('* y variable is : ' , y , '\n')
      cat('* Check to see if ', y , 'is a BINARY variable ... ',  '\n')
      if(h2o.isfactor(final_df_h2o[,y])){
        fam = "binomial"
        cat('*', y , 'is a BINARY variable.' , '\n')
      } else {
        fam = "gaussian"
        cat('*', y , 'is a CONTINIOUS variable.' , '\n')
      }
      
      cat('* Run Ensemble and Best for y : ', y, ', predict and combine residuals','\n')
      {
        cat('* Run Ensemble and Best , Iteration = ', i , '\n')
        {
          cat('* Fit GLM/Ensemble on y : ', y,'\n')
          {
            resyX.pool.Best <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
            GLM_fit <- h2o.glm(model_id = paste("GLM", y, "iequals",as.character(i), sep = "_"), 
                               alpha = Results_Grid$GLM_alpha[which(Results_Grid$DML_iteration == i 
                                                                    & Results_Grid$target_name == y 
                                                                    & Results_Grid$method_name == "GLM")], 
                               lambda = Results_Grid$GLM_lambda[which(Results_Grid$DML_iteration == i 
                                                                      & Results_Grid$target_name == y 
                                                                      & Results_Grid$method_name == "GLM")], 
                               x = predictor_list, 
                               y = y, 
                               training_frame = final_df_h2o, 
                               fold_column="fold_numbers",
                               keep_cross_validation_predictions = TRUE,
                               family = fam,
                               seed = as.integer(Results_ML_Performance_temp[which(Results_ML_Performance_temp$method_name == "GLM" & Results_ML_Performance_temp$target_name == y),]['seed']),
                               standardize = standardize_param_GLM, 
                               lambda_search = lambda_search_param_GLM,
                               interaction_pairs = interaction_pairs_param_GLM, 
                               interactions = interactions_param_GLM
            )
            
            MSE.xval.Best <- h2o.mse(GLM_fit, xval = TRUE)
            MSE.train.Best <- h2o.mse(GLM_fit, train = TRUE)
            Model.Best <- "GLM"
            cat('* Create Vector of residuls of y = ', y ,'\n')
            {
              if(h2o.isfactor(final_df_h2o[,y])){
                resyX.pool.Best <- (h2o.asnumeric(final_df_h2o[,y]) - h2o.getFrame(GLM_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
                cat('*', y, 'is a BINARY variable.' , '\n')
              } else {
                resyX.pool.Best <- (h2o.asnumeric(final_df_h2o[,y]) - h2o.getFrame(GLM_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
                cat('*', y, 'is a CONTINIOUS variable.' , '\n')
              }
            }
          }
          
          cat('* Fit DRF/Ensemble on y : ', y,'\n')
          {
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
                                        seed = as.integer(Results_ML_Performance_temp[which(Results_ML_Performance_temp$method_name == "DRF" & Results_ML_Performance_temp$target_name == y),]['seed']), 
                                        stopping_rounds = stopping_rounds_param_DRF,
                                        stopping_metric = stopping_metric_param_DRF, 
                                        stopping_tolerance = stopping_tolerance_param_DRF,
                                        min_split_improvement = min_split_improvement_param_DRF
            )
            if(MSE.xval.Best > h2o.mse(DRF_fit, xval = TRUE)){
              MSE.xval.Best <- h2o.mse(DRF_fit, xval = TRUE)
              MSE.train.Best <- h2o.mse(DRF_fit, train = TRUE)
              Model.Best <- "DRF"
              cat('* Create Vector of residuls of y = ', y ,'\n')
              {
                if(h2o.isfactor(final_df_h2o[,y])){
                  resyX.pool.Best <- (h2o.asnumeric(final_df_h2o[,y]) - h2o.getFrame(DRF_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
                  cat('*', y, 'is a BINARY variable.' , '\n')
                } else {
                  resyX.pool.Best <- (h2o.asnumeric(final_df_h2o[,y]) - h2o.getFrame(DRF_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
                  cat('*', y, 'is a CONTINIOUS variable.' , '\n')
                }
              }
            }
          }
          
          cat('* Fit GBM/Ensemble on y : ', y,'\n')
          {
            GBM_fit <- h2o.gbm(model_id = paste("GBM", y, "iequals",as.character(i), sep = "_"), 
                               learn_rate = Results_Grid$GBM_learn_rate[which(Results_Grid$DML_iteration == i
                                                                              & Results_Grid$target_name == y
                                                                              & Results_Grid$method_name == "GBM")], 
                               ntrees = Results_Grid$GBM_ntrees[which(Results_Grid$DML_iteration == i
                                                                      & Results_Grid$target_name == y
                                                                      & Results_Grid$method_name == "GBM")], 
                               max_depth = Results_Grid$GBM_max_depth[which(Results_Grid$DML_iteration == i
                                                                            & Results_Grid$target_name == y
                                                                            & Results_Grid$method_name == "GBM")], 
                               min_rows = Results_Grid$GBM_min_rows[which(Results_Grid$DML_iteration == i
                                                                          & Results_Grid$target_name == y
                                                                          & Results_Grid$method_name == "GBM")], 
                               nbins = Results_Grid$GBM_nbins[which(Results_Grid$DML_iteration == i
                                                                    & Results_Grid$target_name == y
                                                                    & Results_Grid$method_name == "GBM")], 
                               x = predictor_list, 
                               y = y, 
                               training_frame = final_df_h2o, 
                               fold_column="fold_numbers",
                               keep_cross_validation_predictions = TRUE,
                               seed = as.integer(Results_ML_Performance_temp[which(Results_ML_Performance_temp$method_name == "GBM" & Results_ML_Performance_temp$target_name == y),]['seed']), 
                               learn_rate_annealing = learn_rate_annealing_param_GBM, 
                               stopping_rounds = stopping_rounds_param_GBM, 
                               stopping_metric = stopping_metric_param_GBM, 
                               stopping_tolerance = stopping_tolerance_param_GBM, 
                               min_split_improvement = min_split_improvement_param_GBM
            )
            if(MSE.xval.Best > h2o.mse(GBM_fit, xval = TRUE)){
              MSE.xval.Best <- h2o.mse(GBM_fit, xval = TRUE)
              MSE.train.Best <- h2o.mse(GBM_fit, train = TRUE)
              Model.Best <- "GBM"
              cat('* Create Vector of residuls of y = ', y ,'\n')
              {
                if(h2o.isfactor(final_df_h2o[,y])){
                  resyX.pool.Best <- (h2o.asnumeric(final_df_h2o[,y]) - h2o.getFrame(GBM_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
                  cat('*', y, 'is a BINARY variable.' , '\n')
                } else {
                  resyX.pool.Best <- (h2o.asnumeric(final_df_h2o[,y]) - h2o.getFrame(GBM_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
                  cat('*', y, 'is a CONTINIOUS variable.' , '\n')
                }
              }
            }
          }
          
          cat('* Fit DL/Ensemble on y : ', y,'\n')
          {
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
                                       seed = as.integer(Results_ML_Performance_temp[which(Results_ML_Performance_temp$method_name == "DL" & Results_ML_Performance_temp$target_name == y),]['seed']), 
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
            if(MSE.xval.Best > h2o.mse(DL_fit, xval = TRUE)){
              MSE.xval.Best <- h2o.mse(DL_fit, xval = TRUE)
              MSE.train.Best <- h2o.mse(DL_fit, train = TRUE)
              Model.Best <- "DL"
              cat('* Create Vector of residuls of y = ', y ,'\n')
              {
                if(h2o.isfactor(final_df_h2o[,y])){
                  resyX.pool.Best <- (h2o.asnumeric(final_df_h2o[,y]) - h2o.getFrame(DL_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
                  cat('*', y, 'is a BINARY variable.' , '\n')
                } else {
                  resyX.pool.Best <- (h2o.asnumeric(final_df_h2o[,y]) - h2o.getFrame(DL_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
                  cat('*', y, 'is a CONTINIOUS variable.' , '\n')
                }
              }
            }
          }
          
          cat('* Fit Ensemble/Ensemble on y : ', y,'\n')
          {
            seed_Ensemble = sample(1:999999, 1, replace=TRUE)
            resyX.pool.Ensemble <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
            Ensemble_fit <- h2o.stackedEnsemble(model_id = paste("Ensemble", y, "iequals",as.character(i), sep = "_"), 
                                                base_models = list(GLM_fit, DRF_fit, GBM_fit, DL_fit), 
                                                x = predictor_list, 
                                                y = y, 
                                                training_frame = final_df_h2o, 
                                                seed = seed_Ensemble, 
                                                keep_levelone_frame = TRUE,
                                                export_checkpoints_dir = NULL,
                                                metalearner_algorithm = c("glm"), 
                                                metalearner_fold_column = "fold_numbers",
                                                metalearner_params = list(lambda = 0, family = fam)
                                                )
            
            levelone_data <- h2o.getFrame(Ensemble_fit@model[["levelone_frame_id"]][["name"]])
            Ensemble_fit_second <- h2o.glm(model_id = paste("Ensemble_fit_second", y, "iequals",as.character(i), sep = "_"),
                                           x = setdiff(h2o.colnames(levelone_data), c("fold_numbers", y)), 
                                           y = y, 
                                           training_frame = levelone_data, 
                                           keep_cross_validation_predictions = TRUE, 
                                           fold_column = "fold_numbers", 
                                           seed = seed_Ensemble, 
                                           lambda = 0, 
                                           family = fam
                                           )
          }
          # cat('* Save the model ... ', '\n')
          # h2o.saveModel(object=Ensemble_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)  
        }
        cat('* Storing Ensemble details ... ', '\n')
        {
          Results_ML_Performance$var_label <- c(Results_ML_Performance$var_label, "Y")
          Results_ML_Performance$DML_iteration <- c(Results_ML_Performance$DML_iteration, i)
          Results_ML_Performance$DML_fold_seed <- c(Results_ML_Performance$DML_fold_seed, seed_h2o)
          Results_ML_Performance$target_name <- c(Results_ML_Performance$target_name, y)
          Results_ML_Performance$method_name <- c(Results_ML_Performance$method_name, "Ensemble")
          Results_ML_Performance$train_MSE <- c(Results_ML_Performance$train_MSE, h2o.performance(Ensemble_fit, train = TRUE, xval = FALSE)@metrics$MSE)
          Results_ML_Performance$xval_MSE <- c(Results_ML_Performance$xval_MSE, h2o.performance(Ensemble_fit, train = FALSE, xval = TRUE)@metrics$MSE)
          Results_ML_Performance$parameters <- c(Results_ML_Performance$parameters, "NA")
          Results_ML_Performance$seed <- c(Results_ML_Performance$seed, seed_Ensemble)
        }
        
        cat('* Create Vector of residuls of y = ', y ,'\n')
        {
          if(h2o.isfactor(final_df_h2o[,y])){
            resyX.pool.Ensemble <- (h2o.asnumeric(final_df_h2o[,y]) - h2o.getFrame(Ensemble_fit_second@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
            cat('*', y, 'is a BINARY variable.' , '\n')
          } else {
            resyX.pool.Ensemble <- (h2o.asnumeric(final_df_h2o[,y]) - h2o.getFrame(Ensemble_fit_second@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
            cat('*', y, 'is a CONTINIOUS variable.' , '\n')
          }
          if(MSE.xval.Best > h2o.mse(Ensemble_fit, xval = TRUE)){
            MSE.xval.Best <- h2o.mse(Ensemble_fit, xval = TRUE)
            MSE.train.Best <- h2o.mse(Ensemble_fit, train = TRUE)
            Model.Best <- "Ensemble"
            resyX.pool.Best <- resyX.pool.Ensemble
          }
          cat('* Cbind with residuals of Y ... ', '\n')
          resYX.pool.Ensemble <- cbind(resYX.pool.Ensemble, resyX.pool.Ensemble)
          resYX.pool.Best <- cbind(resYX.pool.Best, resyX.pool.Best)
        }
        cat('* Storing Best details ... ', '\n')
        {
          Results_ML_Performance$var_label <- c(Results_ML_Performance$var_label, "Y")
          Results_ML_Performance$DML_iteration <- c(Results_ML_Performance$DML_iteration, i)
          Results_ML_Performance$DML_fold_seed <- c(Results_ML_Performance$DML_fold_seed, seed_h2o)
          Results_ML_Performance$target_name <- c(Results_ML_Performance$target_name, y)
          Results_ML_Performance$method_name <- c(Results_ML_Performance$method_name, "Best")
          Results_ML_Performance$train_MSE <- c(Results_ML_Performance$train_MSE, MSE.train.Best)
          Results_ML_Performance$xval_MSE <- c(Results_ML_Performance$xval_MSE, MSE.xval.Best)
          Results_ML_Performance$parameters <- c(Results_ML_Performance$parameters, paste("Best model = ", Model.Best, sep = " "))
          Results_ML_Performance$seed <- c(Results_ML_Performance$seed, NA)
        }
        cat('* Remove objects created by Ensemble on y : ', y ,'\n')
        {
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("GLM", y, "iequals",as.character(i), sep = "_")));
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("DRF", y, "iequals",as.character(i), sep = "_")));
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("GBM", y, "iequals",as.character(i), sep = "_")));
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("DL", y, "iequals",as.character(i), sep = "_")));
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("Ensemble", y, "iequals",as.character(i), sep = "_")));
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("Ensemble_fit_second", y, "iequals",as.character(i), sep = "_")));
          h2o.rm(levelone_data)
          print(h2o.ls())
        }
      }
    }
    
    for(d in D){
      # d=D[1]
      cat('* d variable is : ' , d , '\n')
      cat('* Check to see if ', d , 'is a BINARY variable ... ',  '\n')
      if(h2o.isfactor(final_df_h2o[,d])){
        fam = "binomial"
        cat('*', d , 'is a BINARY variable.' , '\n')
      } else {
        fam = "gaussian"
        cat('*', d , 'is a CONTINIOUS variable.' , '\n')
      }
      
      cat('* Run Ensemble and Best for d : ', d, ', predict and combine residuals','\n')
      {
        cat('* Run Ensemble and Best , Iteration = ', i , '\n')
        {
          cat('* Fit GLM/Ensemble on d : ', d,'\n')
          {
            resdX.pool.Best <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
            GLM_fit <- h2o.glm(model_id = paste("GLM", d, "iequals",as.character(i), sep = "_"), 
                               alpha = Results_Grid$GLM_alpha[which(Results_Grid$DML_iteration == i 
                                                                    & Results_Grid$target_name == d 
                                                                    & Results_Grid$method_name == "GLM")], 
                               lambda = Results_Grid$GLM_lambda[which(Results_Grid$DML_iteration == i 
                                                                      & Results_Grid$target_name == d 
                                                                      & Results_Grid$method_name == "GLM")], 
                               x = predictor_list, 
                               y = d, 
                               training_frame = final_df_h2o, 
                               fold_column="fold_numbers",
                               keep_cross_validation_predictions = TRUE,
                               family = fam,
                               seed = as.integer(Results_ML_Performance_temp[which(Results_ML_Performance_temp$method_name == "GLM" & Results_ML_Performance_temp$target_name == d),]['seed']),
                               standardize = standardize_param_GLM, 
                               lambda_search = lambda_search_param_GLM,
                               interaction_pairs = interaction_pairs_param_GLM, 
                               interactions = interactions_param_GLM
            )
            
            MSE.xval.Best <- h2o.mse(GLM_fit, xval = TRUE)
            MSE.train.Best <- h2o.mse(GLM_fit, train = TRUE)
            Model.Best <- "GLM"
            cat('* Create Vector of residuls of d = ', d ,'\n')
            {
              if(h2o.isfactor(final_df_h2o[,d])){
                resdX.pool.Best <- (h2o.asnumeric(final_df_h2o[,d]) - h2o.getFrame(GLM_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
                cat('*', d, 'is a BINARY variable.' , '\n')
              } else {
                resdX.pool.Best <- (h2o.asnumeric(final_df_h2o[,d]) - h2o.getFrame(GLM_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
                cat('*', d, 'is a CONTINIOUS variable.' , '\n')
              }
            }
          }
          
          cat('* Fit DRF/Ensemble on d : ', d,'\n')
          {
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
                                        seed = as.integer(Results_ML_Performance_temp[which(Results_ML_Performance_temp$method_name == "DRF" & Results_ML_Performance_temp$target_name == d),]['seed']), 
                                        stopping_rounds = stopping_rounds_param_DRF,
                                        stopping_metric = stopping_metric_param_DRF, 
                                        stopping_tolerance = stopping_tolerance_param_DRF,
                                        min_split_improvement = min_split_improvement_param_DRF
            )
            if(MSE.xval.Best > h2o.mse(DRF_fit, xval = TRUE)){
              MSE.xval.Best <- h2o.mse(DRF_fit, xval = TRUE)
              MSE.train.Best <- h2o.mse(DRF_fit, train = TRUE)
              Model.Best <- "DRF"
              cat('* Create Vector of residuls of d = ', d ,'\n')
              {
                if(h2o.isfactor(final_df_h2o[,d])){
                  resdX.pool.Best <- (h2o.asnumeric(final_df_h2o[,d]) - h2o.getFrame(DRF_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
                  cat('*', d, 'is a BINARY variable.' , '\n')
                } else {
                  resdX.pool.Best <- (h2o.asnumeric(final_df_h2o[,d]) - h2o.getFrame(DRF_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
                  cat('*', d, 'is a CONTINIOUS variable.' , '\n')
                }
              }
            }
          }
          
          cat('* Fit GBM/Ensemble on d : ', d,'\n')
          {
            GBM_fit <- h2o.gbm(model_id = paste("GBM", d, "iequals",as.character(i), sep = "_"), 
                               learn_rate = Results_Grid$GBM_learn_rate[which(Results_Grid$DML_iteration == i
                                                                              & Results_Grid$target_name == d
                                                                              & Results_Grid$method_name == "GBM")], 
                               ntrees = Results_Grid$GBM_ntrees[which(Results_Grid$DML_iteration == i
                                                                      & Results_Grid$target_name == d
                                                                      & Results_Grid$method_name == "GBM")], 
                               max_depth = Results_Grid$GBM_max_depth[which(Results_Grid$DML_iteration == i
                                                                            & Results_Grid$target_name == d
                                                                            & Results_Grid$method_name == "GBM")], 
                               min_rows = Results_Grid$GBM_min_rows[which(Results_Grid$DML_iteration == i
                                                                          & Results_Grid$target_name == d
                                                                          & Results_Grid$method_name == "GBM")], 
                               nbins = Results_Grid$GBM_nbins[which(Results_Grid$DML_iteration == i
                                                                    & Results_Grid$target_name == d
                                                                    & Results_Grid$method_name == "GBM")], 
                               x = predictor_list, 
                               y = d, 
                               training_frame = final_df_h2o, 
                               fold_column="fold_numbers",
                               keep_cross_validation_predictions = TRUE,
                               seed = as.integer(Results_ML_Performance_temp[which(Results_ML_Performance_temp$method_name == "GBM" & Results_ML_Performance_temp$target_name == d),]['seed']), 
                               learn_rate_annealing = learn_rate_annealing_param_GBM, 
                               stopping_rounds = stopping_rounds_param_GBM, 
                               stopping_metric = stopping_metric_param_GBM, 
                               stopping_tolerance = stopping_tolerance_param_GBM, 
                               min_split_improvement = min_split_improvement_param_GBM
            )
            if(MSE.xval.Best > h2o.mse(GBM_fit, xval = TRUE)){
              MSE.xval.Best <- h2o.mse(GBM_fit, xval = TRUE)
              MSE.train.Best <- h2o.mse(GBM_fit, train = TRUE)
              Model.Best <- "GBM"
              cat('* Create Vector of residuls of d = ', d ,'\n')
              {
                if(h2o.isfactor(final_df_h2o[,d])){
                  resdX.pool.Best <- (h2o.asnumeric(final_df_h2o[,d]) - h2o.getFrame(GBM_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
                  cat('*', d, 'is a BINARY variable.' , '\n')
                } else {
                  resdX.pool.Best <- (h2o.asnumeric(final_df_h2o[,d]) - h2o.getFrame(GBM_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
                  cat('*', d, 'is a CONTINIOUS variable.' , '\n')
                }
              }
            }
          }
          
          cat('* Fit DL/Ensemble on d : ', d,'\n')
          {
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
                                       seed = as.integer(Results_ML_Performance_temp[which(Results_ML_Performance_temp$method_name == "DL" & Results_ML_Performance_temp$target_name == d),]['seed']), 
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
            if(MSE.xval.Best > h2o.mse(DL_fit, xval = TRUE)){
              MSE.xval.Best <- h2o.mse(DL_fit, xval = TRUE)
              MSE.train.Best <- h2o.mse(DL_fit, train = TRUE)
              Model.Best <- "DL"
              cat('* Create Vector of residuls of d = ', d ,'\n')
              {
                if(h2o.isfactor(final_df_h2o[,d])){
                  resdX.pool.Best <- (h2o.asnumeric(final_df_h2o[,d]) - h2o.getFrame(DL_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
                  cat('*', d, 'is a BINARY variable.' , '\n')
                } else {
                  resdX.pool.Best <- (h2o.asnumeric(final_df_h2o[,d]) - h2o.getFrame(DL_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
                  cat('*', d, 'is a CONTINIOUS variable.' , '\n')
                }
              }
            }
          }
          
          cat('* Fit Ensemble/Ensemble on d : ', d,'\n')
          {
            seed_Ensemble = sample(1:999999, 1, replace=TRUE)
            resdX.pool.Ensemble <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
            Ensemble_fit <- h2o.stackedEnsemble(model_id = paste("Ensemble", d, "iequals",as.character(i), sep = "_"), 
                                                base_models = list(GLM_fit, DRF_fit, GBM_fit, DL_fit), 
                                                x = predictor_list, 
                                                y = d, 
                                                training_frame = final_df_h2o, 
                                                seed = seed_Ensemble, 
                                                keep_levelone_frame = TRUE,
                                                export_checkpoints_dir = NULL,
                                                metalearner_algorithm = c("glm"), 
                                                metalearner_fold_column = "fold_numbers",
                                                metalearner_params = list(lambda = 0, family = fam)
            )
            
            levelone_data <- h2o.getFrame(Ensemble_fit@model[["levelone_frame_id"]][["name"]])
            Ensemble_fit_second <- h2o.glm(model_id = paste("Ensemble_fit_second", d, "iequals",as.character(i), sep = "_"),
                                           x = setdiff(h2o.colnames(levelone_data), c("fold_numbers", d)), 
                                           y = d, 
                                           training_frame = levelone_data, 
                                           keep_cross_validation_predictions = TRUE, 
                                           fold_column = "fold_numbers", 
                                           seed = seed_Ensemble, 
                                           lambda = 0, 
                                           family = fam
            )
          }
          
          # cat('* Save the model ... ', '\n')
          # h2o.saveModel(object=Ensemble_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)  
        }
        cat('* Storing Ensemble details ... ', '\n')
        {
          Results_ML_Performance$var_label <- c(Results_ML_Performance$var_label, "D")
          Results_ML_Performance$DML_iteration <- c(Results_ML_Performance$DML_iteration, i)
          Results_ML_Performance$DML_fold_seed <- c(Results_ML_Performance$DML_fold_seed, seed_h2o)
          Results_ML_Performance$target_name <- c(Results_ML_Performance$target_name, d)
          Results_ML_Performance$method_name <- c(Results_ML_Performance$method_name, "Ensemble")
          Results_ML_Performance$train_MSE <- c(Results_ML_Performance$train_MSE, h2o.performance(Ensemble_fit, train = TRUE, xval = FALSE)@metrics$MSE)
          Results_ML_Performance$xval_MSE <- c(Results_ML_Performance$xval_MSE, h2o.performance(Ensemble_fit, train = FALSE, xval = TRUE)@metrics$MSE)
          Results_ML_Performance$parameters <- c(Results_ML_Performance$parameters, "NA")
          Results_ML_Performance$seed <- c(Results_ML_Performance$seed, seed_Ensemble)
        }
        
        cat('* Create Vector of residuls of d = ', d ,'\n')
        {
          if(h2o.isfactor(final_df_h2o[,d])){
            resdX.pool.Ensemble <- (h2o.asnumeric(final_df_h2o[,d]) - h2o.getFrame(Ensemble_fit_second@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
            cat('*', d, 'is a BINARY variable.' , '\n')
          } else {
            resdX.pool.Ensemble <- (h2o.asnumeric(final_df_h2o[,d]) - h2o.getFrame(Ensemble_fit_second@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
            cat('*', d, 'is a CONTINIOUS variable.' , '\n')
          }
          if(MSE.xval.Best > h2o.mse(Ensemble_fit, xval = TRUE)){
            MSE.xval.Best <- h2o.mse(Ensemble_fit, xval = TRUE)
            MSE.train.Best <- h2o.mse(Ensemble_fit, train = TRUE)
            Model.Best <- "Ensemble"
            resdX.pool.Best <- resdX.pool.Ensemble
          }
          cat('* Cbind with residuals of D ... ', '\n')
          resDX.pool.Ensemble <- cbind(resDX.pool.Ensemble, resdX.pool.Ensemble)
          resDX.pool.Best <- cbind(resDX.pool.Best, resdX.pool.Best)
        }
        cat('* Storing Best details ... ', '\n')
        {
          Results_ML_Performance$var_label <- c(Results_ML_Performance$var_label, "D")
          Results_ML_Performance$DML_iteration <- c(Results_ML_Performance$DML_iteration, i)
          Results_ML_Performance$DML_fold_seed <- c(Results_ML_Performance$DML_fold_seed, seed_h2o)
          Results_ML_Performance$target_name <- c(Results_ML_Performance$target_name, d)
          Results_ML_Performance$method_name <- c(Results_ML_Performance$method_name, "Best")
          Results_ML_Performance$train_MSE <- c(Results_ML_Performance$train_MSE, MSE.train.Best)
          Results_ML_Performance$xval_MSE <- c(Results_ML_Performance$xval_MSE, MSE.xval.Best)
          Results_ML_Performance$parameters <- c(Results_ML_Performance$parameters, paste("Best model = ", Model.Best, sep = " "))
          Results_ML_Performance$seed <- c(Results_ML_Performance$seed, NA)
        }
        cat('* Remove objects created by Ensemble on d : ', d ,'\n')
        {
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("GLM", d, "iequals",as.character(i), sep = "_")));
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("DRF", d, "iequals",as.character(i), sep = "_")));
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("GBM", d, "iequals",as.character(i), sep = "_")));
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("DL", d, "iequals",as.character(i), sep = "_")));
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("Ensemble", d, "iequals",as.character(i), sep = "_")));
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("Ensemble_fit_second", d, "iequals",as.character(i), sep = "_")));
          h2o.rm(levelone_data)
          print(h2o.ls())
        }
      }
    }
    
    for(z in Z){
      # z=Z[1]
      cat('* z variable is : ' , z , '\n')
      cat('* Check to see if ', z , 'is a BINARY variable ... ',  '\n')
      if(h2o.isfactor(final_df_h2o[,z])){
        fam = "binomial"
        cat('*', z , 'is a BINARY variable.' , '\n')
      } else {
        fam = "gaussian"
        cat('*', z , 'is a CONTINIOUS variable.' , '\n')
      }
      
      cat('* Run Ensemble and Best for z : ', z, ', predict and combine residuals','\n')
      {
        cat('* Run Ensemble and Best , Iteration = ', i , '\n')
        {
          cat('* Fit GLM/Ensemble on z : ', z,'\n')
          {
            reszX.pool.Best <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
            GLM_fit <- h2o.glm(model_id = paste("GLM", z, "iequals",as.character(i), sep = "_"), 
                               alpha = Results_Grid$GLM_alpha[which(Results_Grid$DML_iteration == i 
                                                                    & Results_Grid$target_name == z 
                                                                    & Results_Grid$method_name == "GLM")], 
                               lambda = Results_Grid$GLM_lambda[which(Results_Grid$DML_iteration == i 
                                                                      & Results_Grid$target_name == z 
                                                                      & Results_Grid$method_name == "GLM")], 
                               x = predictor_list, 
                               y = z, 
                               training_frame = final_df_h2o, 
                               fold_column="fold_numbers",
                               keep_cross_validation_predictions = TRUE,
                               family = fam,
                               seed = as.integer(Results_ML_Performance_temp[which(Results_ML_Performance_temp$method_name == "GLM" & Results_ML_Performance_temp$target_name == z),]['seed']),
                               standardize = standardize_param_GLM, 
                               lambda_search = lambda_search_param_GLM,
                               interaction_pairs = interaction_pairs_param_GLM, 
                               interactions = interactions_param_GLM
            )
            
            MSE.xval.Best <- h2o.mse(GLM_fit, xval = TRUE)
            MSE.train.Best <- h2o.mse(GLM_fit, train = TRUE)
            Model.Best <- "GLM"
            cat('* Create Vector of residuls of z = ', z ,'\n')
            {
              if(h2o.isfactor(final_df_h2o[,z])){
                reszX.pool.Best <- (h2o.asnumeric(final_df_h2o[,z]) - h2o.getFrame(GLM_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
                cat('*', z, 'is a BINARY variable.' , '\n')
              } else {
                reszX.pool.Best <- (h2o.asnumeric(final_df_h2o[,z]) - h2o.getFrame(GLM_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
                cat('*', z, 'is a CONTINIOUS variable.' , '\n')
              }
            }
          }
          
          cat('* Fit DRF/Ensemble on z : ', z,'\n')
          {
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
                                        seed = as.integer(Results_ML_Performance_temp[which(Results_ML_Performance_temp$method_name == "DRF" & Results_ML_Performance_temp$target_name == z),]['seed']), 
                                        stopping_rounds = stopping_rounds_param_DRF,
                                        stopping_metric = stopping_metric_param_DRF, 
                                        stopping_tolerance = stopping_tolerance_param_DRF,
                                        min_split_improvement = min_split_improvement_param_DRF
            )
            if(MSE.xval.Best > h2o.mse(DRF_fit, xval = TRUE)){
              MSE.xval.Best <- h2o.mse(DRF_fit, xval = TRUE)
              MSE.train.Best <- h2o.mse(DRF_fit, train = TRUE)
              Model.Best <- "DRF"
              cat('* Create Vector of residuls of z = ', z ,'\n')
              {
                if(h2o.isfactor(final_df_h2o[,z])){
                  reszX.pool.Best <- (h2o.asnumeric(final_df_h2o[,z]) - h2o.getFrame(DRF_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
                  cat('*', z, 'is a BINARY variable.' , '\n')
                } else {
                  reszX.pool.Best <- (h2o.asnumeric(final_df_h2o[,z]) - h2o.getFrame(DRF_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
                  cat('*', z, 'is a CONTINIOUS variable.' , '\n')
                }
              }
            }
          }
          
          cat('* Fit GBM/Ensemble on z : ', z,'\n')
          {
            GBM_fit <- h2o.gbm(model_id = paste("GBM", z, "iequals",as.character(i), sep = "_"), 
                               learn_rate = Results_Grid$GBM_learn_rate[which(Results_Grid$DML_iteration == i
                                                                              & Results_Grid$target_name == z
                                                                              & Results_Grid$method_name == "GBM")], 
                               ntrees = Results_Grid$GBM_ntrees[which(Results_Grid$DML_iteration == i
                                                                      & Results_Grid$target_name == z
                                                                      & Results_Grid$method_name == "GBM")], 
                               max_depth = Results_Grid$GBM_max_depth[which(Results_Grid$DML_iteration == i
                                                                            & Results_Grid$target_name == z
                                                                            & Results_Grid$method_name == "GBM")], 
                               min_rows = Results_Grid$GBM_min_rows[which(Results_Grid$DML_iteration == i
                                                                          & Results_Grid$target_name == z
                                                                          & Results_Grid$method_name == "GBM")], 
                               nbins = Results_Grid$GBM_nbins[which(Results_Grid$DML_iteration == i
                                                                    & Results_Grid$target_name == z
                                                                    & Results_Grid$method_name == "GBM")], 
                               x = predictor_list, 
                               y = z, 
                               training_frame = final_df_h2o, 
                               fold_column="fold_numbers",
                               keep_cross_validation_predictions = TRUE,
                               seed = as.integer(Results_ML_Performance_temp[which(Results_ML_Performance_temp$method_name == "GBM" & Results_ML_Performance_temp$target_name == z),]['seed']), 
                               learn_rate_annealing = learn_rate_annealing_param_GBM, 
                               stopping_rounds = stopping_rounds_param_GBM, 
                               stopping_metric = stopping_metric_param_GBM, 
                               stopping_tolerance = stopping_tolerance_param_GBM, 
                               min_split_improvement = min_split_improvement_param_GBM
            )
            if(MSE.xval.Best > h2o.mse(GBM_fit, xval = TRUE)){
              MSE.xval.Best <- h2o.mse(GBM_fit, xval = TRUE)
              MSE.train.Best <- h2o.mse(GBM_fit, train = TRUE)
              Model.Best <- "GBM"
              cat('* Create Vector of residuls of z = ', z ,'\n')
              {
                if(h2o.isfactor(final_df_h2o[,z])){
                  reszX.pool.Best <- (h2o.asnumeric(final_df_h2o[,z]) - h2o.getFrame(GBM_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
                  cat('*', z, 'is a BINARY variable.' , '\n')
                } else {
                  reszX.pool.Best <- (h2o.asnumeric(final_df_h2o[,z]) - h2o.getFrame(GBM_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
                  cat('*', z, 'is a CONTINIOUS variable.' , '\n')
                }
              }
            }
          }
          
          cat('* Fit DL/Ensemble on z : ', z,'\n')
          {
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
                                       seed = as.integer(Results_ML_Performance_temp[which(Results_ML_Performance_temp$method_name == "DL" & Results_ML_Performance_temp$target_name == z),]['seed']), 
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
            if(MSE.xval.Best > h2o.mse(DL_fit, xval = TRUE)){
              MSE.xval.Best <- h2o.mse(DL_fit, xval = TRUE)
              MSE.train.Best <- h2o.mse(DL_fit, train = TRUE)
              Model.Best <- "DL"
              cat('* Create Vector of residuls of z = ', z ,'\n')
              {
                if(h2o.isfactor(final_df_h2o[,z])){
                  reszX.pool.Best <- (h2o.asnumeric(final_df_h2o[,z]) - h2o.getFrame(DL_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
                  cat('*', z, 'is a BINARY variable.' , '\n')
                } else {
                  reszX.pool.Best <- (h2o.asnumeric(final_df_h2o[,z]) - h2o.getFrame(DL_fit@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
                  cat('*', z, 'is a CONTINIOUS variable.' , '\n')
                }
              }
            }
          }
          
          cat('* Fit Ensemble/Ensemble on z : ', z,'\n')
          {
            seed_Ensemble = sample(1:999999, 1, replace=TRUE)
            reszX.pool.Ensemble <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
            Ensemble_fit <- h2o.stackedEnsemble(model_id = paste("Ensemble", z, "iequals",as.character(i), sep = "_"), 
                                                base_models = list(GLM_fit, DRF_fit, GBM_fit, DL_fit), 
                                                x = predictor_list, 
                                                y = z, 
                                                training_frame = final_df_h2o, 
                                                seed = seed_Ensemble, 
                                                keep_levelone_frame = TRUE,
                                                export_checkpoints_dir = NULL,
                                                metalearner_algorithm = c("glm"), 
                                                metalearner_fold_column = "fold_numbers",
                                                metalearner_params = list(lambda = 0, family = fam)
            )
            
            levelone_data <- h2o.getFrame(Ensemble_fit@model[["levelone_frame_id"]][["name"]])
            Ensemble_fit_second <- h2o.glm(model_id = paste("Ensemble_fit_second", z, "iequals",as.character(i), sep = "_"),
                                           x = setdiff(h2o.colnames(levelone_data), c("fold_numbers", z)), 
                                           y = z, 
                                           training_frame = levelone_data, 
                                           keep_cross_validation_predictions = TRUE, 
                                           fold_column = "fold_numbers", 
                                           seed = seed_Ensemble, 
                                           lambda = 0, 
                                           family = fam
            )
          }
          
          # cat('* Save the model ... ', '\n')
          # h2o.saveModel(object=Ensemble_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)  
        }
        cat('* Storing Ensemble details ... ', '\n')
        {
          Results_ML_Performance$var_label <- c(Results_ML_Performance$var_label, "Z")
          Results_ML_Performance$DML_iteration <- c(Results_ML_Performance$DML_iteration, i)
          Results_ML_Performance$DML_fold_seed <- c(Results_ML_Performance$DML_fold_seed, seed_h2o)
          Results_ML_Performance$target_name <- c(Results_ML_Performance$target_name, z)
          Results_ML_Performance$method_name <- c(Results_ML_Performance$method_name, "Ensemble")
          Results_ML_Performance$train_MSE <- c(Results_ML_Performance$train_MSE, h2o.performance(Ensemble_fit, train = TRUE, xval = FALSE)@metrics$MSE)
          Results_ML_Performance$xval_MSE <- c(Results_ML_Performance$xval_MSE, h2o.performance(Ensemble_fit, train = FALSE, xval = TRUE)@metrics$MSE)
          Results_ML_Performance$parameters <- c(Results_ML_Performance$parameters, "NA")
          Results_ML_Performance$seed <- c(Results_ML_Performance$seed, seed_Ensemble)
        }
        
        cat('* Create Vector of residuls of z = ', z ,'\n')
        {
          if(h2o.isfactor(final_df_h2o[,z])){
            reszX.pool.Ensemble <- (h2o.asnumeric(final_df_h2o[,z]) - h2o.getFrame(Ensemble_fit_second@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
            cat('*', z, 'is a BINARY variable.' , '\n')
          } else {
            reszX.pool.Ensemble <- (h2o.asnumeric(final_df_h2o[,z]) - h2o.getFrame(Ensemble_fit_second@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
            cat('*', z, 'is a CONTINIOUS variable.' , '\n')
          }
          if(MSE.xval.Best > h2o.mse(Ensemble_fit, xval = TRUE)){
            MSE.xval.Best <- h2o.mse(Ensemble_fit, xval = TRUE)
            MSE.train.Best <- h2o.mse(Ensemble_fit, train = TRUE)
            Model.Best <- "Ensemble"
            reszX.pool.Best <- reszX.pool.Ensemble
          }
          cat('* Cbind with residuals of Z ... ', '\n')
          resZX.pool.Ensemble <- cbind(resZX.pool.Ensemble, reszX.pool.Ensemble)
          resZX.pool.Best <- cbind(resZX.pool.Best, reszX.pool.Best)
        }
        cat('* Storing Ensemble details ... ', '\n')
        {
          Results_ML_Performance$var_label <- c(Results_ML_Performance$var_label, "Z")
          Results_ML_Performance$DML_iteration <- c(Results_ML_Performance$DML_iteration, i)
          Results_ML_Performance$DML_fold_seed <- c(Results_ML_Performance$DML_fold_seed, seed_h2o)
          Results_ML_Performance$target_name <- c(Results_ML_Performance$target_name, z)
          Results_ML_Performance$method_name <- c(Results_ML_Performance$method_name, "Best")
          Results_ML_Performance$train_MSE <- c(Results_ML_Performance$train_MSE, MSE.train.Best)
          Results_ML_Performance$xval_MSE <- c(Results_ML_Performance$xval_MSE, MSE.xval.Best)
          Results_ML_Performance$parameters <- c(Results_ML_Performance$parameters, paste("Best model = ", Model.Best, sep = " "))
          Results_ML_Performance$seed <- c(Results_ML_Performance$seed, NA)
        }
        cat('* Remove objects created by Ensemble on z : ', z ,'\n')
        {
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("GLM", z, "iequals",as.character(i), sep = "_")));
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("DRF", z, "iequals",as.character(i), sep = "_")));
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("GBM", z, "iequals",as.character(i), sep = "_")));
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("DL", z, "iequals",as.character(i), sep = "_")));
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("Ensemble", z, "iequals",as.character(i), sep = "_")));
          h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("Ensemble_fit_second", z, "iequals",as.character(i), sep = "_")));
          h2o.rm(levelone_data)
          print(h2o.ls())
        }
      }
    }
    
  }
  
  cat('* GMM with the Ensemble Method , Iteration = ', i , '\n')
  {
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
        resYX.Ensemble <- resYX.pool.Ensemble[which(fold.pool == j),] %>% as.matrix()
        resDX.Ensemble <- resDX.pool.Ensemble[which(fold.pool == j),] %>% as.matrix()
        resZX.Ensemble <- resZX.pool.Ensemble[which(fold.pool == j),] %>% as.matrix()
        gmm1 <- NULL
        gmm1 <- gmm(resYX.Ensemble~resDX.Ensemble-1,~resZX.Ensemble-1,vcov="iid")
        cat('* DML 1, fold', j , 'results summary', '\n')
        print(gmm1 %>% summary())
        
        for(lc in 1:length(ncol(resDX.Ensemble))){
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
        Results_DML$method_name <- c(Results_DML$method_name, "Ensemble")
        for(lc in 1:length(colnames(resDX.pool.Ensemble))){
          Results_DML[[(4+2*lc)]] <- c(Results_DML[[(4+2*lc)]], as.numeric(DML1_combine[[(-1+2*lc)]]))
          Results_DML[[(5+2*lc)]] <- c(Results_DML[[(5+2*lc)]], as.numeric(sqrt(DML1_combine[[(2*lc)]])))
        }
      }
    }
    
    cat('* Perform GMM estimation for DML2, Iteration = ', i , '\n')
    {
      gmm2 <- NULL
      gmm2 <- gmm(resYX.pool.Ensemble~resDX.pool.Ensemble-1,~resZX.pool.Ensemble-1,vcov="iid")
      cat('* DML 2 results summary', '\n')
      print(gmm2 %>% summary())
      cat('* Storing Results of DML 2 ... ', '\n')
      {
        Results_DML$DML_iteration <- c(Results_DML$DML_iteration, i)
        Results_DML$DML_fold_seed <- c(Results_DML$DML_fold_seed, seed_h2o)
        Results_DML$DML_type <- c(Results_DML$DML_type, "DML 2")
        Results_DML$K_folds <- c(Results_DML$K_folds, K.folds)
        Results_DML$method_name <- c(Results_DML$method_name, "Ensemble")
        for(lc in 1:length(ncol(resDX.pool.Ensemble))){
          Results_DML[[(4+2*lc)]] <- c(Results_DML[[(4+2*lc)]], as.numeric(as.numeric(gmm2$coefficients[lc])))
          Results_DML[[(5+2*lc)]] <- c(Results_DML[[(5+2*lc)]], as.numeric(sqrt(as.numeric(gmm2$vcov[lc,lc]))))
        }
      }
    }  
  }

  cat('* GMM with the Best Method , Iteration = ', i , '\n')
  {
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
        resYX.Best <- resYX.pool.Best[which(fold.pool == j),] %>% as.matrix()
        resDX.Best <- resDX.pool.Best[which(fold.pool == j),] %>% as.matrix()
        resZX.Best <- resZX.pool.Best[which(fold.pool == j),] %>% as.matrix()
        gmm1 <- NULL
        gmm1 <- gmm(resYX.Best~resDX.Best-1,~resZX.Best-1,vcov="iid")
        cat('* DML 1, fold', j , 'results summary', '\n')
        print(gmm1 %>% summary())
        
        for(lc in 1:length(ncol(resDX.Best))){
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
        Results_DML$method_name <- c(Results_DML$method_name, "Best")
        for(lc in 1:length(colnames(resDX.pool.Best))){
          Results_DML[[(4+2*lc)]] <- c(Results_DML[[(4+2*lc)]], as.numeric(DML1_combine[[(-1+2*lc)]]))
          Results_DML[[(5+2*lc)]] <- c(Results_DML[[(5+2*lc)]], as.numeric(sqrt(DML1_combine[[(2*lc)]])))
        }
      }
    }
    
    cat('* Perform GMM estimation for DML2, Iteration = ', i , '\n')
    {
      gmm2 <- NULL
      gmm2 <- gmm(resYX.pool.Best~resDX.pool.Best-1,~resZX.pool.Best-1,vcov="iid")
      cat('* DML 2 results summary', '\n')
      print(gmm2 %>% summary())
      cat('* Storing Results of DML 2 ... ', '\n')
      {
        Results_DML$DML_iteration <- c(Results_DML$DML_iteration, i)
        Results_DML$DML_fold_seed <- c(Results_DML$DML_fold_seed, seed_h2o)
        Results_DML$DML_type <- c(Results_DML$DML_type, "DML 2")
        Results_DML$K_folds <- c(Results_DML$K_folds, K.folds)
        Results_DML$method_name <- c(Results_DML$method_name, "Best")
        for(lc in 1:length(colnames(resDX.pool.Best))){
          Results_DML[[(4+2*lc)]] <- c(Results_DML[[(4+2*lc)]], as.numeric(as.numeric(gmm2$coefficients[lc])))
          Results_DML[[(5+2*lc)]] <- c(Results_DML[[(5+2*lc)]], as.numeric(sqrt(as.numeric(gmm2$vcov[lc,lc]))))
        }
      }
    }
  }
  cat('* Remove unnecessary object, Iteration = ', i , '\n')
  {
    h2o.rm(GLM_fit);
    h2o.rm(DRF_fit);
    h2o.rm(GBM_fit);
    h2o.rm(DL_fit);
    h2o.rm(Ensemble_fit);
    h2o.rm(Ensemble_fit_second);
    rm(fold.pool); gc();
    rm(resYX.pool.Ensemble); gc();
    rm(resDX.pool.Ensemble); gc();
    rm(resZX.pool.Ensemble); gc();
    rm(resYX.pool.Best); gc();
    rm(resDX.pool.Best); gc();
    rm(resZX.pool.Best); gc();
    rm(resyX.pool.Best); gc();
    rm(resyX.pool.Ensemble); gc();
    rm(resdX.pool.Best); gc();
    rm(resdX.pool.Ensemble); gc();
    rm(reszX.pool.Best); gc();
    rm(reszX.pool.Ensemble); gc();
    rm(Results_ML_Performance_temp); gc();
    rm(resYX.Ensemble); gc();
    rm(resDX.Ensemble); gc();
    rm(resZX.Ensemble); gc();
    rm(resYX.Best); gc();
    rm(resDX.Best); gc();
    rm(resZX.Best); gc();
    rm(DML1_combine); gc();
    rm(MSE.xval.Best); gc();
    rm(MSE.train.Best); gc();
    rm(Model.Best); gc();
    rm(seed_Ensemble); gc();
    rm(gmm1); gc();
    rm(gmm2); gc();
  }
}

