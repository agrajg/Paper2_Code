cat('* Procedure for Ensemble.' , '\n')
{
  cat('* Initilizing cross fit predicted outcome to pool for DML ... ', '\n') 
  {
    fold.pool  <- final_df_h2o$fold_numbers %>% h2o.asnumeric() %>% as.matrix()
    resYX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
    resDX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
    resZX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
  }
  
  cat('* Estimate nuisance function with Ensemble.', '\n')
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
      
      cat('* Run Ensemble for y : ', y, ', predict and combine residuals','\n')
      {
        cat('* Run Ensemble ... ', '\n')
        {
          cat('* Fit GLM/Ensemble on y : ', y,'\n')
          {
            seed = sample(1:999999, 1, replace=TRUE)
            GLM_fit <- h2o.glm(model_id = paste("GLM", y, "iequals",as.character(i), sep = "_"), 
                               alpha = Grid_Results$GLM_alpha[which(Grid_Results$DML_iteration == i 
                                                                    & Grid_Results$target_name == y 
                                                                    & Grid_Results$method_name == "GLM")], 
                               lambda = Grid_Results$GLM_lambda[which(Grid_Results$DML_iteration == i 
                                                                      & Grid_Results$target_name == y 
                                                                      & Grid_Results$method_name == "GLM")], 
                               x = predictor_list, 
                               y = y, 
                               training_frame = final_df_h2o, 
                               fold_column="fold_numbers",
                               keep_cross_validation_predictions = TRUE,
                               family = fam,
                               seed = seed,
                               standardize = standardize_param_GLM, 
                               lambda_search = lambda_search_param_GLM,
                               interaction_pairs = interaction_pairs_param_GLM, 
                               interactions = interactions_param_GLM
            )
            
          }
          
          cat('* Fit DRF/Ensemble on y : ', y,'\n')
          {
            seed = sample(1:999999, 1, replace=TRUE)
            DRF_fit <- h2o.randomForest(model_id = paste("DRF", y, "iequals",as.character(i), sep = "_"), 
                                        ntrees = Grid_Results$DRF_ntrees[which(Grid_Results$DML_iteration == i
                                                                               & Grid_Results$target_name == y
                                                                               & Grid_Results$method_name == "DRF")], 
                                        max_depth = Grid_Results$DRF_max_depth[which(Grid_Results$DML_iteration == i 
                                                                                     & Grid_Results$target_name == y 
                                                                                     & Grid_Results$method_name == "DRF")], 
                                        min_rows = Grid_Results$DRF_min_rows[which(Grid_Results$DML_iteration == i 
                                                                                   & Grid_Results$target_name == y 
                                                                                   & Grid_Results$method_name == "DRF")], 
                                        nbins = Grid_Results$DRF_nbins[which(Grid_Results$DML_iteration == i 
                                                                             & Grid_Results$target_name == y 
                                                                             & Grid_Results$method_name == "DRF")], 
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
            
          }
          
          cat('* Fit GBM/Ensemble on y : ', y,'\n')
          {
            seed = sample(1:999999, 1, replace=TRUE)
            GBM_fit <- h2o.gbm(model_id = paste("GBM", y, "iequals",as.character(i), sep = "_"), 
                               learn_rate = Grid_Results$GBM_learn_rate[which(Grid_Results$DML_iteration == i
                                                                              & Grid_Results$target_name == y
                                                                              & Grid_Results$method_name == "GBM")], 
                               ntrees = Grid_Results$GBM_ntrees[which(Grid_Results$DML_iteration == i
                                                                      & Grid_Results$target_name == y
                                                                      & Grid_Results$method_name == "GBM")], 
                               max_depth = Grid_Results$GBM_max_depth[which(Grid_Results$DML_iteration == i
                                                                            & Grid_Results$target_name == y
                                                                            & Grid_Results$method_name == "GBM")], 
                               min_rows = Grid_Results$GBM_min_rows[which(Grid_Results$DML_iteration == i
                                                                          & Grid_Results$target_name == y
                                                                          & Grid_Results$method_name == "GBM")], 
                               nbins = Grid_Results$GBM_nbins[which(Grid_Results$DML_iteration == i
                                                                    & Grid_Results$target_name == y
                                                                    & Grid_Results$method_name == "GBM")], 
                               x = predictor_list, 
                               y = y, 
                               training_frame = final_df_h2o, 
                               fold_column="fold_numbers",
                               keep_cross_validation_predictions = TRUE,
                               seed = seed, 
                               learn_rate_annealing = learn_rate_annealing_param_GBM, 
                               stopping_rounds = stopping_rounds_param_GBM, 
                               stopping_metric = stopping_metric_param_GBM, 
                               stopping_tolerance = stopping_tolerance_param_GBM, 
                               min_split_improvement = min_split_improvement_param_GBM
            )
          }
          
          cat('* Fit DL/Ensemble on y : ', y,'\n')
          {
            seed = sample(1:999999, 1, replace=TRUE)
            DL_fit <- h2o.deeplearning(model_id = paste("DL", y, "iequals",as.character(i), sep = "_"), 
                                       hidden = as.numeric(strsplit(as.character(Grid_Results$DL_hidden[which(Grid_Results$DML_iteration == i
                                                                                                              & Grid_Results$target_name == y
                                                                                                              & Grid_Results$method_name == "DL")]),
                                                                    split = "_")[[1]]),
                                       activation = as.character(Grid_Results$DL_activation[which(Grid_Results$DML_iteration == i
                                                                                                  & Grid_Results$target_name == y
                                                                                                  & Grid_Results$method_name == "DL")]), 
                                       l1 = Grid_Results$DL_l1[which(Grid_Results$DML_iteration == i
                                                                     & Grid_Results$target_name == y
                                                                     & Grid_Results$method_name == "DL")], 
                                       l2 = Grid_Results$DL_l2[which(Grid_Results$DML_iteration == i
                                                                     & Grid_Results$target_name == y
                                                                     & Grid_Results$method_name == "DL")], 
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
          }
          
          cat('* Fit Ensemble/Ensemble on y : ', y,'\n')
          {
            seed = sample(1:999999, 1, replace=TRUE)
            Ensemble_fit <- h2o.stackedEnsemble(model_id = paste("Ensemble", y, "iequals",as.character(i), sep = "_"), 
                                                base_models = list(GLM_fit, DRF_fit, GBM_fit, DL_fit), 
                                                x = predictor_list, 
                                                y = y, 
                                                training_frame = final_df_h2o, 
                                                seed = seed, 
                                                keep_levelone_frame = TRUE,
                                                export_checkpoints_dir = NULL,
                                                metalearner_algorithm = c("glm"), 
                                                metalearner_fold_column = "fold_numbers",
                                                metalearner_params = list(lambda = 0, family = fam))
            
            levelone_data <- h2o.getFrame(Ensemble_fit@model[["levelone_frame_id"]][["name"]])
            Ensemble_fit_second <- h2o.glm(model_id = paste("Ensemble_fit_second", y, "iequals",as.character(i), sep = "_"),
                                           x = setdiff(h2o.colnames(levelone_data), c("fold_numbers", y)), 
                                           y = y, 
                                           training_frame = levelone_data, 
                                           keep_cross_validation_predictions = TRUE, 
                                           fold_column = "fold_numbers", 
                                           seed = seed, 
                                           lambda = 0, 
                                           family = fam)
          }
          
          resyX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
          # cat('* Save the model ... ', '\n')
          # h2o.saveModel(object=Ensemble_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)  
        }
        cat('* Storing Ensemble details ... ', '\n')
        {
          ML_perf_Results$var_label <- c(ML_perf_Results$var_label, "Y")
          ML_perf_Results$DML_iteration <- c(ML_perf_Results$DML_iteration, i)
          ML_perf_Results$DML_fold_seed <- c(ML_perf_Results$DML_fold_seed, seed_h2o)
          ML_perf_Results$target_name <- c(ML_perf_Results$target_name, y)
          ML_perf_Results$method_name <- c(ML_perf_Results$method_name, "Ensemble")
          ML_perf_Results$train_MSE <- c(ML_perf_Results$train_MSE, h2o.performance(Ensemble_fit, train = TRUE, xval = FALSE)@metrics$MSE)
          ML_perf_Results$xval_MSE <- c(ML_perf_Results$xval_MSE, h2o.performance(Ensemble_fit, train = FALSE, xval = TRUE)@metrics$MSE)
          ML_perf_Results$parameters <- c(ML_perf_Results$parameters, "-")
          ML_perf_Results$seed <- c(ML_perf_Results$seed, seed)
        }
        
        cat('* Create Vector of residuls of y = ', y ,'\n')
        {
          if(h2o.isfactor(final_df_h2o[,y])){
            resyX.pool <- (h2o.asnumeric(final_df_h2o[,y]) - h2o.getFrame(Ensemble_fit_second@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
            cat('*', y, 'is a BINARY variable.' , '\n')
          } else {
            resyX.pool <- (h2o.asnumeric(final_df_h2o[,y]) - h2o.getFrame(Ensemble_fit_second@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
            cat('*', y, 'is a CONTINIOUS variable.' , '\n')
          }
          cat('* Cbind with residuals of Y ... ', '\n')
          resYX.pool <- cbind(resYX.pool, resyX.pool)
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
      
      cat('* Run Ensemble for d : ', d, ', predict and combine residuals','\n')
      {
        cat('* Run Ensemble ... ', '\n')
        {
          
          cat('* Fit GLM/Ensemble on d : ', d,'\n')
          {
            seed = sample(1:999999, 1, replace=TRUE)
            GLM_fit <- h2o.glm(model_id = paste("GLM", d, "iequals",as.character(i), sep = "_"), 
                               alpha = Grid_Results$GLM_alpha[which(Grid_Results$DML_iteration == i 
                                                                    & Grid_Results$target_name == d 
                                                                    & Grid_Results$method_name == "GLM")], 
                               lambda = Grid_Results$GLM_lambda[which(Grid_Results$DML_iteration == i 
                                                                      & Grid_Results$target_name == d 
                                                                      & Grid_Results$method_name == "GLM")], 
                               x = predictor_list, 
                               y = d, 
                               training_frame = final_df_h2o, 
                               fold_column="fold_numbers",
                               keep_cross_validation_predictions = TRUE,
                               family = fam,
                               seed = seed,
                               standardize = standardize_param_GLM, 
                               lambda_search = lambda_search_param_GLM,
                               interaction_pairs = interaction_pairs_param_GLM, 
                               interactions = interactions_param_GLM
            )
          }
          
          cat('* Fit DRF/Ensemble on d : ', d,'\n')
          {
            seed = sample(1:999999, 1, replace=TRUE)
            DRF_fit <- h2o.randomForest(model_id = paste("DRF", d, "iequals",as.character(i), sep = "_"), 
                                        ntrees = Grid_Results$DRF_ntrees[which(Grid_Results$DML_iteration == i
                                                                               & Grid_Results$target_name == d
                                                                               & Grid_Results$method_name == "DRF")], 
                                        max_depth = Grid_Results$DRF_max_depth[which(Grid_Results$DML_iteration == i 
                                                                                     & Grid_Results$target_name == d 
                                                                                     & Grid_Results$method_name == "DRF")], 
                                        min_rows = Grid_Results$DRF_min_rows[which(Grid_Results$DML_iteration == i 
                                                                                   & Grid_Results$target_name == d 
                                                                                   & Grid_Results$method_name == "DRF")], 
                                        nbins = Grid_Results$DRF_nbins[which(Grid_Results$DML_iteration == i 
                                                                             & Grid_Results$target_name == d 
                                                                             & Grid_Results$method_name == "DRF")], 
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
          }
          
          cat('* Fit GBM/Ensemble on d : ', d,'\n')
          {
            seed = sample(1:999999, 1, replace=TRUE)
            GBM_fit <- h2o.gbm(model_id = paste("GBM", d, "iequals",as.character(i), sep = "_"), 
                               learn_rate = Grid_Results$GBM_learn_rate[which(Grid_Results$DML_iteration == i
                                                                              & Grid_Results$target_name == d
                                                                              & Grid_Results$method_name == "GBM")], 
                               ntrees = Grid_Results$GBM_ntrees[which(Grid_Results$DML_iteration == i
                                                                      & Grid_Results$target_name == d
                                                                      & Grid_Results$method_name == "GBM")], 
                               max_depth = Grid_Results$GBM_max_depth[which(Grid_Results$DML_iteration == i
                                                                            & Grid_Results$target_name == d
                                                                            & Grid_Results$method_name == "GBM")], 
                               min_rows = Grid_Results$GBM_min_rows[which(Grid_Results$DML_iteration == i
                                                                          & Grid_Results$target_name == d
                                                                          & Grid_Results$method_name == "GBM")], 
                               nbins = Grid_Results$GBM_nbins[which(Grid_Results$DML_iteration == i
                                                                    & Grid_Results$target_name == d
                                                                    & Grid_Results$method_name == "GBM")], 
                               x = predictor_list, 
                               y = d, 
                               training_frame = final_df_h2o, 
                               fold_column="fold_numbers",
                               keep_cross_validation_predictions = TRUE,
                               seed = seed, 
                               learn_rate_annealing = learn_rate_annealing_param_GBM, 
                               stopping_rounds = stopping_rounds_param_GBM, 
                               stopping_metric = stopping_metric_param_GBM, 
                               stopping_tolerance = stopping_tolerance_param_GBM, 
                               min_split_improvement = min_split_improvement_param_GBM
            )  
          }
          
          cat('* Fit DL/Ensemble on d : ', d,'\n')
          {
            seed = sample(1:999999, 1, replace=TRUE)
            DL_fit <- h2o.deeplearning(model_id = paste("DL", d, "iequals",as.character(i), sep = "_"), 
                                       hidden = as.numeric(strsplit(as.character(Grid_Results$DL_hidden[which(Grid_Results$DML_iteration == i
                                                                                                              & Grid_Results$target_name == d
                                                                                                              & Grid_Results$method_name == "DL")]),
                                                                    split = "_")[[1]]),
                                       activation = as.character(Grid_Results$DL_activation[which(Grid_Results$DML_iteration == i
                                                                                                  & Grid_Results$target_name == d
                                                                                                  & Grid_Results$method_name == "DL")]), 
                                       l1 = Grid_Results$DL_l1[which(Grid_Results$DML_iteration == i
                                                                     & Grid_Results$target_name == d
                                                                     & Grid_Results$method_name == "DL")], 
                                       l2 = Grid_Results$DL_l2[which(Grid_Results$DML_iteration == i
                                                                     & Grid_Results$target_name == d
                                                                     & Grid_Results$method_name == "DL")], 
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
            
            
          }
          
          cat('* Fit Ensemble/Ensemble on d : ', d,'\n')
          {
            seed = sample(1:999999, 1, replace=TRUE)
            Ensemble_fit <- h2o.stackedEnsemble(model_id = paste("Ensemble", d, "iequals",as.character(i), sep = "_"), 
                                                base_models = list(GLM_fit, DRF_fit, GBM_fit, DL_fit), 
                                                x = predictor_list, 
                                                y = d, 
                                                training_frame = final_df_h2o, 
                                                seed = seed, 
                                                keep_levelone_frame = TRUE,
                                                export_checkpoints_dir = NULL,
                                                metalearner_algorithm = c("glm"), 
                                                metalearner_fold_column = "fold_numbers",
                                                metalearner_params = list(lambda = 0, family = fam))
            
            levelone_data <- h2o.getFrame(Ensemble_fit@model[["levelone_frame_id"]][["name"]])
            Ensemble_fit_second <- h2o.glm(model_id = paste("Ensemble_fit_second", d, "iequals",as.character(i), sep = "_"),
                                           x = setdiff(h2o.colnames(levelone_data), c("fold_numbers", d)), 
                                           y = d, 
                                           training_frame = levelone_data, 
                                           keep_cross_validation_predictions = TRUE, 
                                           fold_column = "fold_numbers", 
                                           seed = seed, 
                                           lambda = 0, 
                                           family = fam)
          }
          
          resdX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
          # cat('* Save the model ... ', '\n')
          # h2o.saveModel(object=Ensemble_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)  
        }
        
        cat('* Storing Ensemble details ... ', '\n')
        {
          ML_perf_Results$var_label <- c(ML_perf_Results$var_label, "D")
          ML_perf_Results$DML_iteration <- c(ML_perf_Results$DML_iteration, i)
          ML_perf_Results$DML_fold_seed <- c(ML_perf_Results$DML_fold_seed, seed_h2o)
          ML_perf_Results$target_name <- c(ML_perf_Results$target_name, d)
          ML_perf_Results$method_name <- c(ML_perf_Results$method_name, "Ensemble")
          ML_perf_Results$train_MSE <- c(ML_perf_Results$train_MSE, h2o.performance(Ensemble_fit, train = TRUE, xval = FALSE)@metrics$MSE)
          ML_perf_Results$xval_MSE <- c(ML_perf_Results$xval_MSE, h2o.performance(Ensemble_fit, train = FALSE, xval = TRUE)@metrics$MSE)
          ML_perf_Results$parameters <- c(ML_perf_Results$parameters, "-")
          ML_perf_Results$seed <- c(ML_perf_Results$seed, seed)
        }
        
        cat('* Create Vector of residuls of d = ', d ,'\n')
        {
          if(h2o.isfactor(final_df_h2o[,d])){
            resdX.pool <- (h2o.asnumeric(final_df_h2o[,d]) - h2o.getFrame(Ensemble_fit_second@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
            cat('*', d, 'is a BINARY variable.' , '\n')
          } else {
            resdX.pool <- (h2o.asnumeric(final_df_h2o[,d]) - h2o.getFrame(Ensemble_fit_second@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
            cat('*', d, 'is a CONTINIOUS variable.' , '\n')
          }
          cat('* Cbind with residuals of D ... ', '\n')
          resDX.pool <- cbind(resDX.pool, resdX.pool)
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
      
      cat('* Run Ensemble for z : ', z, ', predict and combine residuals','\n')
      {
        cat('* Run Ensemble ... ', '\n')
        {
          cat('* Fit GLM/Ensemble on z : ', z,'\n')
          {
            seed = sample(1:999999, 1, replace=TRUE)
            GLM_fit <- h2o.glm(model_id = paste("GLM", z , "iequals" ,as.character(i), sep = "_"), 
                               alpha = Grid_Results$GLM_alpha[which(Grid_Results$DML_iteration == i 
                                                                    & Grid_Results$target_name == z 
                                                                    & Grid_Results$method_name == "GLM")], 
                               lambda = Grid_Results$GLM_lambda[which(Grid_Results$DML_iteration == i 
                                                                      & Grid_Results$target_name == z 
                                                                      & Grid_Results$method_name == "GLM")], 
                               x = predictor_list, 
                               y = z, 
                               training_frame = final_df_h2o, 
                               fold_column="fold_numbers",
                               keep_cross_validation_predictions = TRUE,
                               family = fam,
                               seed = seed,
                               standardize = standardize_param_GLM , 
                               lambda_search = lambda_search_param_GLM,
                               interaction_pairs = interaction_pairs_param_GLM, 
                               interactions = interactions_param_GLM
            )
            
          }
          
          cat('* Fit DRF/Ensemble on z : ', z,'\n')
          {
            seed = sample(1:999999, 1, replace=TRUE)
            DRF_fit <- h2o.randomForest(model_id = paste("DRF", z, "iequals",as.character(i), sep = "_"), 
                                        ntrees = Grid_Results$DRF_ntrees[which(Grid_Results$DML_iteration == i
                                                                               & Grid_Results$target_name == z
                                                                               & Grid_Results$method_name == "DRF")], 
                                        max_depth = Grid_Results$DRF_max_depth[which(Grid_Results$DML_iteration == i 
                                                                                     & Grid_Results$target_name == z 
                                                                                     & Grid_Results$method_name == "DRF")], 
                                        min_rows = Grid_Results$DRF_min_rows[which(Grid_Results$DML_iteration == i 
                                                                                   & Grid_Results$target_name == z 
                                                                                   & Grid_Results$method_name == "DRF")], 
                                        nbins = Grid_Results$DRF_nbins[which(Grid_Results$DML_iteration == i 
                                                                             & Grid_Results$target_name == z 
                                                                             & Grid_Results$method_name == "DRF")], 
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
          }
          
          cat('* Fit GBM/Ensemble on z : ', z,'\n')
          {
            seed = sample(1:999999, 1, replace=TRUE)
            GBM_fit <- h2o.gbm(model_id = paste("GBM", z, "iequals",as.character(i), sep = "_"), 
                               learn_rate = Grid_Results$GBM_learn_rate[which(Grid_Results$DML_iteration == i
                                                                              & Grid_Results$target_name == z
                                                                              & Grid_Results$method_name == "GBM")], 
                               ntrees = Grid_Results$GBM_ntrees[which(Grid_Results$DML_iteration == i
                                                                      & Grid_Results$target_name == z
                                                                      & Grid_Results$method_name == "GBM")], 
                               max_depth = Grid_Results$GBM_max_depth[which(Grid_Results$DML_iteration == i
                                                                            & Grid_Results$target_name == z
                                                                            & Grid_Results$method_name == "GBM")], 
                               min_rows = Grid_Results$GBM_min_rows[which(Grid_Results$DML_iteration == i
                                                                          & Grid_Results$target_name == z
                                                                          & Grid_Results$method_name == "GBM")], 
                               nbins = Grid_Results$GBM_nbins[which(Grid_Results$DML_iteration == i
                                                                    & Grid_Results$target_name == z
                                                                    & Grid_Results$method_name == "GBM")], 
                               x = predictor_list, 
                               y = z, 
                               training_frame = final_df_h2o, 
                               fold_column="fold_numbers",
                               keep_cross_validation_predictions = TRUE,
                               seed = seed, 
                               learn_rate_annealing = learn_rate_annealing_param_GBM, 
                               stopping_rounds = stopping_rounds_param_GBM, 
                               stopping_metric = stopping_metric_param_GBM, 
                               stopping_tolerance = stopping_tolerance_param_GBM, 
                               min_split_improvement = min_split_improvement_param_GBM
            )
            
          }
          
          cat('* Fit DL/Ensemble on z : ', z,'\n')
          {
            seed = sample(1:999999, 1, replace=TRUE)
            DL_fit <- h2o.deeplearning(model_id = paste("DL", z, "iequals",as.character(i), sep = "_"), 
                                       hidden = as.numeric(strsplit(as.character(Grid_Results$DL_hidden[which(Grid_Results$DML_iteration == i
                                                                                                              & Grid_Results$target_name == z
                                                                                                              & Grid_Results$method_name == "DL")]),
                                                                    split = "_")[[1]]),
                                       activation = as.character(Grid_Results$DL_activation[which(Grid_Results$DML_iteration == i
                                                                                                  & Grid_Results$target_name == z
                                                                                                  & Grid_Results$method_name == "DL")]), 
                                       l1 = Grid_Results$DL_l1[which(Grid_Results$DML_iteration == i
                                                                     & Grid_Results$target_name == z
                                                                     & Grid_Results$method_name == "DL")], 
                                       l2 = Grid_Results$DL_l2[which(Grid_Results$DML_iteration == i
                                                                     & Grid_Results$target_name == z
                                                                     & Grid_Results$method_name == "DL")], 
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
          }
          
          cat('* Fit Ensemble/Ensemble on z : ', z,'\n')
          {
            seed = sample(1:999999, 1, replace=TRUE)
            Ensemble_fit <- h2o.stackedEnsemble(model_id = paste("Ensemble", z, "iequals",as.character(i), sep = "_"), 
                                                base_models = list(GLM_fit, DRF_fit, GBM_fit, DL_fit), 
                                                x = predictor_list, 
                                                y = z, 
                                                training_frame = final_df_h2o, 
                                                seed = seed, 
                                                keep_levelone_frame = TRUE,
                                                export_checkpoints_dir = NULL,
                                                metalearner_algorithm = c("glm"), 
                                                metalearner_fold_column = "fold_numbers",
                                                metalearner_params = list(lambda = 0, family = fam))
            
            levelone_data <- h2o.getFrame(Ensemble_fit@model[["levelone_frame_id"]][["name"]])
            Ensemble_fit_second <- h2o.glm(model_id = paste("Ensemble_fit_second", z, "iequals",as.character(i), sep = "_"),
                                           x = setdiff(h2o.colnames(levelone_data), c("fold_numbers", z)), 
                                           y = z, 
                                           training_frame = levelone_data, 
                                           keep_cross_validation_predictions = TRUE, 
                                           fold_column = "fold_numbers", 
                                           seed = seed, 
                                           lambda = 0, 
                                           family = fam)
          }
          
          reszX.pool <- matrix(nrow = h2o.nrow(final_df_h2o), ncol = 0)
          # cat('* Save the model ... ', '\n')
          # h2o.saveModel(object=Ensemble_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)  
        }
        
        cat('* Storing Ensemble details ... ', '\n')
        {
          ML_perf_Results$var_label <- c(ML_perf_Results$var_label, "Z")
          ML_perf_Results$DML_iteration <- c(ML_perf_Results$DML_iteration, i)
          ML_perf_Results$DML_fold_seed <- c(ML_perf_Results$DML_fold_seed, seed_h2o)
          ML_perf_Results$target_name <- c(ML_perf_Results$target_name, z)
          ML_perf_Results$method_name <- c(ML_perf_Results$method_name, "Ensemble")
          ML_perf_Results$train_MSE <- c(ML_perf_Results$train_MSE, h2o.performance(Ensemble_fit, train = TRUE, xval = FALSE)@metrics$MSE)
          ML_perf_Results$xval_MSE <- c(ML_perf_Results$xval_MSE, h2o.performance(Ensemble_fit, train = FALSE, xval = TRUE)@metrics$MSE)
          ML_perf_Results$parameters <- c(ML_perf_Results$parameters, "-")
          ML_perf_Results$seed <- c(ML_perf_Results$seed, seed)
        }
        
        cat('* Create Vector of residuls of z = ', Z ,'\n')
        {
          if(h2o.isfactor(final_df_h2o[,z])){
            reszX.pool <- (h2o.asnumeric(final_df_h2o[,z]) - h2o.getFrame(Ensemble_fit_second@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])$p1) %>% as.matrix()
            cat('*', z, 'is a BINARY variable.' , '\n')
          } else {
            reszX.pool <- (h2o.asnumeric(final_df_h2o[,z]) - h2o.getFrame(Ensemble_fit_second@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])) %>% as.matrix()
            cat('*', z, 'is a CONTINIOUS variable.' , '\n')
          }
          cat('* Cbind with residuals of Z ... ', '\n')
          resZX.pool <- cbind(resZX.pool, reszX.pool)
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
  
  cat('* Perform GMM estimation for DML1.', '\n')
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
      resYX <- resYX.pool[which(fold.pool == j),]
      resDX <- resDX.pool[which(fold.pool == j),]
      resZX <- resZX.pool[which(fold.pool == j),]
      gmm1 <- gmm(resYX~resDX-1,~resZX-1,vcov="iid")
      cat('* DML 1, fold', j , 'results summary', '\n')
      print(gmm1 %>% summary())
      
      for(lc in 1:length(colnames(resDX.pool))){
        DML1_combine[[(-1+2*lc)]] <- DML1_combine[[(-1+2*lc)]] + as.numeric((1/K.folds)  *(gmm1$coefficients[paste("resDX", colnames(resDX)[lc], sep = "")]))
        DML1_combine[[(2*lc)]]    <- DML1_combine[[(2*lc)]]    + as.numeric((1/K.folds^2)*(gmm1$vcov[paste("resDX", colnames(resDX)[lc], sep = ""),paste("resDX", colnames(resDX)[lc], sep = "")]))
      }
      rm(gmm1); gc();
    }
    
    cat('* Storing Results of DML 1 ... ', '\n')
    {
      Results_DML$DML_iteration <- c(Results_DML$DML_iteration, i)
      Results_DML$DML_fold_seed <- c(Results_DML$DML_fold_seed, seed_h2o)
      Results_DML$DML_type <- c(Results_DML$DML_type, "DML 1")
      Results_DML$K_folds <- c(Results_DML$K_folds, K.folds)
      Results_DML$method_name <- c(Results_DML$method_name, "Ensemble")
      for(lc in 1:length(colnames(resDX.pool))){
        Results_DML[[(4+2*lc)]] <- c(Results_DML[[(4+2*lc)]], as.numeric(DML1_combine[[(-1+2*lc)]]))
        Results_DML[[(5+2*lc)]] <- c(Results_DML[[(5+2*lc)]], as.numeric(sqrt(DML1_combine[[(2*lc)]])))
      }
    }
  }
  
  cat('* Perform GMM estimation for DML2.', '\n')
  {
    gmm2 <- gmm(resYX.pool~resDX.pool-1,~resZX.pool-1,vcov="iid")
    cat('* DML 2 results summary', '\n')
    print(gmm2 %>% summary())
    cat('* Storing Results of DML 1 ... ', '\n')
    {
      Results_DML$DML_iteration <- c(Results_DML$DML_iteration, i)
      Results_DML$DML_fold_seed <- c(Results_DML$DML_fold_seed, seed_h2o)
      Results_DML$DML_type <- c(Results_DML$DML_type, "DML 2")
      Results_DML$K_folds <- c(Results_DML$K_folds, K.folds)
      Results_DML$method_name <- c(Results_DML$method_name, "Ensemble")
      for(lc in 1:length(colnames(resDX.pool))){
        Results_DML[[(4+2*lc)]] <- c(Results_DML[[(4+2*lc)]], as.numeric(gmm2$coefficients[paste("resDX.pool", colnames(resDX.pool)[lc], sep = "")]))
        Results_DML[[(5+2*lc)]] <- c(Results_DML[[(5+2*lc)]], as.numeric(sqrt(gmm2$vcov[paste("resDX.pool", colnames(resDX.pool)[lc], sep = ""),paste("resDX.pool", colnames(resDX.pool)[lc], sep = "")])))
      }
    }
    rm(gmm2); gc();
  }
}

