cat('* Train the grid on datause for each target, Iteration = ', i , '\n')
for(target in target_list){
  # target = "lprice_per_person"
  # target = "prod_week1_0"
  
  cat('* Check to see if ',target, 'is a BINARY variable ... ',  '\n')
  if(h2o.isfactor(final_df_h2o[,target])){
    fam = "binomial"
    cat('*', target, 'is a BINARY variable.' , '\n')
  } else {
    fam = "gaussian"
    cat('*', target, 'is a CONTINIOUS variable.' , '\n')
  }
  
  cat('* Formulate GLM grid for target : ', target, ', Iteration = ', i , '\n')
  {
    method = "GLM"
    grid_seed_GLM = sample(1:999999, 1, replace=TRUE)
    grid_GLM <- h2o.grid(algorithm = "glm", 
                         hyper_params = hparams_GLM, 
                         search_criteria = search_criteria,
                         grid_id = "grid_GLM" , 
                         x = predictor_list, 
                         y = target, 
                         training_frame = final_df_h2o, 
                         fold_column="fold_numbers",
                         family = fam,
                         seed = grid_seed_GLM,
                         standardize = standardize_param_GLM, 
                         lambda_search = lambda_search_param_GLM,
                         interaction_pairs = interaction_pairs_param_GLM, 
                         interactions = interactions_param_GLM)
    cat('* Evaluating GLM grid performance ... ')
    grid_GLM_perf <- h2o.getGrid(grid_id = "grid_GLM", sort_by = "MSE" , decreasing = FALSE)
    cat('* Grid GLM models : ')
    print(grid_GLM_perf)
    cat('* Picking up the best GLM model ... ',  '\n')
    best_GLM <- h2o.getModel(grid_GLM_perf@model_ids[[1]])
    cat('* Calculating best GLM performance on train valid and test(dataout)... ', '\n')
    best_GLM_perf_train <- h2o.performance(best_GLM, train = TRUE)
    best_GLM_perf_xval <- h2o.performance(best_GLM, xval = TRUE)
  }
  cat('* Writing the GLM performance list ... ', '\n')
  {
    Results_Grid$DML_iteration <- c(Results_Grid$DML_iteration, i)
    Results_Grid$DML_fold_seed <- c(Results_Grid$DML_fold_seed, seed_h2o)
    Results_Grid$target_name  <- c(Results_Grid$target_name, target)
    Results_Grid$method_name  <- c(Results_Grid$method_name, method)
    Results_Grid$train_MSE  <- c(Results_Grid$train_MSE, best_GLM_perf_train@metrics["MSE"][[1]])
    Results_Grid$xval_MSE <- c(Results_Grid$xval_MSE, best_GLM_perf_xval@metrics["MSE"][[1]])
    Results_Grid$GLM_alpha  <- c(Results_Grid$GLM_alpha, best_GLM@allparameters$alpha)
    Results_Grid$GLM_lambda  <- c(Results_Grid$GLM_lambda, best_GLM@allparameters$lambda)
    Results_Grid$DRF_ntrees  <- c(Results_Grid$DRF_ntrees, NA)
    Results_Grid$DRF_max_depth   <- c(Results_Grid$DRF_max_depth, NA)
    Results_Grid$DRF_min_rows   <- c(Results_Grid$DRF_min_rows, NA)
    Results_Grid$DRF_nbins <- c(Results_Grid$DRF_nbins, NA)
    Results_Grid$GBM_learn_rate  <- c(Results_Grid$GBM_learn_rate, NA)
    Results_Grid$GBM_ntrees  <- c(Results_Grid$GBM_ntrees, NA)
    Results_Grid$GBM_max_depth  <- c(Results_Grid$GBM_max_depth, NA)
    Results_Grid$GBM_min_rows  <- c(Results_Grid$GBM_min_rows, NA)
    Results_Grid$GBM_nbins  <- c(Results_Grid$GBM_nbins, NA)
    Results_Grid$DL_hidden  <- c(Results_Grid$DL_hidden, "")
    Results_Grid$DL_activation  <- c(Results_Grid$DL_activation, NA)
    Results_Grid$DL_l1  <- c(Results_Grid$DL_l1, NA)
    Results_Grid$DL_l2 <- c(Results_Grid$DL_l2, NA) 
    Results_Grid$grid_seed <- c(Results_Grid$grid_seed, grid_seed_GLM)  
  }
  cat('* Remove GLM objects that are not needed ... ' , '\n')
  {
    h2o.rm("grid_GLM"); rm(grid_GLM);rm(grid_GLM_perf);
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "grid_GLM"));        
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "GLM_model_R"));
    h2o.rm(best_GLM); rm(best_GLM_perf_train); rm(best_GLM_perf_xval);
    print(h2o.ls())
  }
  
  cat('* Formulate DRF grid for target : ', target, ', Iteration = ', i , '\n')
  {
    method = "DRF"
    grid_seed_DRF = sample(1:999999, 1, replace=TRUE)
    grid_DRF <- h2o.grid(algorithm = "randomForest", 
                         hyper_params = hparams_DRF, 
                         search_criteria = search_criteria,
                         grid_id = "grid_DRF" , 
                         x = predictor_list, 
                         y = target, 
                         training_frame = final_df_h2o, 
                         fold_column="fold_numbers",
                         seed = grid_seed_DRF,
                         stopping_rounds = stopping_rounds_param_DRF,  
                         stopping_metric = stopping_metric_param_DRF, 
                         stopping_tolerance = stopping_tolerance_param_DRF,  
                         min_split_improvement = min_split_improvement_param_DRF
                         )
    cat('* Evaluating DRF grid performance ... ')
    grid_DRF_perf <- h2o.getGrid(grid_id = "grid_DRF", sort_by = "MSE" , decreasing = FALSE)
    cat('* Grid DRF models : ')
    print(grid_DRF_perf)
    cat('* Picking up the best DRF model ... ',  '\n')
    best_DRF <- h2o.getModel(grid_DRF_perf@model_ids[[1]])
    cat('* Calculating best DRF performance on train valid and test(dataout)... ', '\n')
    best_DRF_perf_train <- h2o.performance(best_DRF, train = TRUE)
    best_DRF_perf_xval <- h2o.performance(best_DRF, xval = TRUE)
  }
  cat('* Writing the DRF performance list ... ', '\n')
  {
    Results_Grid$DML_iteration <- c(Results_Grid$DML_iteration, i)
    Results_Grid$DML_fold_seed <- c(Results_Grid$DML_fold_seed, seed_h2o)
    Results_Grid$target_name  <- c(Results_Grid$target_name, target)
    Results_Grid$method_name  <- c(Results_Grid$method_name, method)
    Results_Grid$train_MSE  <- c(Results_Grid$train_MSE, best_DRF_perf_train@metrics["MSE"][[1]])
    Results_Grid$xval_MSE <- c(Results_Grid$xval_MSE, best_DRF_perf_xval@metrics["MSE"][[1]])
    Results_Grid$GLM_alpha  <- c(Results_Grid$GLM_alpha, NA)
    Results_Grid$GLM_lambda  <- c(Results_Grid$GLM_lambda, NA)
    Results_Grid$DRF_ntrees  <- c(Results_Grid$DRF_ntrees, best_DRF@allparameters$ntrees)
    Results_Grid$DRF_max_depth   <- c(Results_Grid$DRF_max_depth, best_DRF@allparameters$max_depth)
    Results_Grid$DRF_min_rows   <- c(Results_Grid$DRF_min_rows, best_DRF@allparameters$min_rows)
    Results_Grid$DRF_nbins <- c(Results_Grid$DRF_nbins, best_DRF@allparameters$nbins)
    Results_Grid$GBM_learn_rate  <- c(Results_Grid$GBM_learn_rate, NA)
    Results_Grid$GBM_ntrees  <- c(Results_Grid$GBM_ntrees, NA)
    Results_Grid$GBM_max_depth  <- c(Results_Grid$GBM_max_depth, NA)
    Results_Grid$GBM_min_rows  <- c(Results_Grid$GBM_min_rows, NA)
    Results_Grid$GBM_nbins  <- c(Results_Grid$GBM_nbins, NA)
    Results_Grid$DL_hidden  <- c(Results_Grid$DL_hidden, "")
    Results_Grid$DL_activation  <- c(Results_Grid$DL_activation, NA)
    Results_Grid$DL_l1  <- c(Results_Grid$DL_l1, NA)
    Results_Grid$DL_l2 <- c(Results_Grid$DL_l2, NA) 
    Results_Grid$grid_seed <- c(Results_Grid$grid_seed, grid_seed_DRF)  
  }
  cat('* Remove DRF objects that are not needed ... ' , '\n')
  {
    h2o.rm("grid_DRF"); rm(grid_DRF);rm(grid_DRF_perf);
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "grid_DRF"));
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "DRF_model_R"));
    h2o.rm(best_DRF); rm(best_DRF_perf_train); rm(best_DRF_perf_xval);
    print(h2o.ls())
  }
  
  cat('* Formulate GBM grid for target : ', target, ', Iteration = ', i , '\n')
  {
    method = "GBM"
    grid_seed_GBM = sample(1:999999, 1, replace=TRUE)
    grid_GBM <- h2o.grid(algorithm = "gbm", 
                         hyper_params = hparams_GBM, 
                         search_criteria = search_criteria,
                         grid_id = "grid_GBM" , 
                         x = predictor_list, 
                         y = target, 
                         training_frame = final_df_h2o, 
                         fold_column="fold_numbers", 
                         seed = grid_seed_GBM,
                         learn_rate_annealing = learn_rate_annealing_param_GBM, 
                         stopping_rounds = stopping_rounds_param_GBM, 
                         stopping_metric = stopping_metric_param_GBM, 
                         stopping_tolerance = stopping_tolerance_param_GBM, 
                         min_split_improvement = min_split_improvement_param_GBM)
    cat('* Evaluating GBM grid performance ... ')
    grid_GBM_perf <- h2o.getGrid(grid_id = "grid_GBM", sort_by = "MSE" , decreasing = FALSE)
    cat('* Grid GBM models : ')
    print(grid_GBM_perf)
    cat('* Picking up the best GBM model ... ',  '\n')
    best_GBM <- h2o.getModel(grid_GBM_perf@model_ids[[1]])
    cat('* Calculating best GBM performance on train valid and test(dataout)... ', '\n')
    best_GBM_perf_train <- h2o.performance(best_GBM, train = TRUE)
    best_GBM_perf_xval <- h2o.performance(best_GBM, xval = TRUE)
  }
  cat('* Writing the GBM performance list ... ', '\n')
  {
    Results_Grid$DML_iteration <- c(Results_Grid$DML_iteration, i)
    Results_Grid$DML_fold_seed <- c(Results_Grid$DML_fold_seed, seed_h2o)
    Results_Grid$target_name  <- c(Results_Grid$target_name, target)
    Results_Grid$method_name  <- c(Results_Grid$method_name, method)
    Results_Grid$train_MSE  <- c(Results_Grid$train_MSE, best_GBM_perf_train@metrics["MSE"][[1]])
    Results_Grid$xval_MSE <- c(Results_Grid$xval_MSE, best_GBM_perf_xval@metrics["MSE"][[1]])
    Results_Grid$GLM_alpha  <- c(Results_Grid$GLM_alpha, NA)
    Results_Grid$GLM_lambda  <- c(Results_Grid$GLM_lambda, NA)
    Results_Grid$DRF_ntrees  <- c(Results_Grid$DRF_ntrees, NA)
    Results_Grid$DRF_max_depth   <- c(Results_Grid$DRF_max_depth, NA)
    Results_Grid$DRF_min_rows   <- c(Results_Grid$DRF_min_rows, NA)
    Results_Grid$DRF_nbins <- c(Results_Grid$DRF_nbins, NA)
    Results_Grid$GBM_learn_rate  <- c(Results_Grid$GBM_learn_rate, best_GBM@allparameters$learn_rate)
    Results_Grid$GBM_ntrees  <- c(Results_Grid$GBM_ntrees, best_GBM@allparameters$ntrees)
    Results_Grid$GBM_max_depth  <- c(Results_Grid$GBM_max_depth, best_GBM@allparameters$max_depth)
    Results_Grid$GBM_min_rows  <- c(Results_Grid$GBM_min_rows, best_GBM@allparameters$min_rows)
    Results_Grid$GBM_nbins  <- c(Results_Grid$GBM_nbins, best_GBM@allparameters$nbins)
    Results_Grid$DL_hidden  <- c(Results_Grid$DL_hidden, "")
    Results_Grid$DL_activation  <- c(Results_Grid$DL_activation, NA)
    Results_Grid$DL_l1  <- c(Results_Grid$DL_l1, NA)
    Results_Grid$DL_l2 <- c(Results_Grid$DL_l2, NA) 
    Results_Grid$grid_seed <- c(Results_Grid$grid_seed, grid_seed_GBM)  
  }
  cat('* Remove GBM objects that are not needed ... ' , '\n')
  {
    h2o.rm("grid_GBM"); rm(grid_GBM);rm(grid_GBM_perf);
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "grid_GBM"));
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "GBM_model_R"));
    h2o.rm(best_GBM); rm(best_GBM_perf_train); rm(best_GBM_perf_xval);
    print(h2o.ls())
  }
  
  cat('* Formulate DL grid for target : ', target, ', Iteration = ', i , '\n')
  {
    method = "DL"
    grid_seed_DL = sample(1:999999, 1, replace=TRUE)
    grid_DL <- h2o.grid(algorithm = "deeplearning", 
                        hyper_params = hparams_DL, 
                        search_criteria = search_criteria,
                        grid_id = "grid_DL" , 
                        x = predictor_list, 
                        y = target, 
                        training_frame = final_df_h2o, 
                        fold_column="fold_numbers",
                        seed = grid_seed_DL,
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
    
    cat('* Evaluating DL grid performance ... ')
    grid_DL_perf <- h2o.getGrid(grid_id = "grid_DL", sort_by = "MSE" , decreasing = FALSE)
    cat('* Grid DL models : ')
    print(grid_DL_perf)
    cat('* Picking up the best DL model ... ',  '\n')
    best_DL <- h2o.getModel(grid_DL_perf@model_ids[[1]])
    cat('* Calculating best GLM performance on train valid and test(dataout)... ', '\n')
    best_DL_perf_train <- h2o.performance(best_DL, train = TRUE)
    best_DL_perf_xval <- h2o.performance(best_DL, xval = TRUE)
  }
  cat('* Writing the DL performance list ... ', '\n')
  {
    Results_Grid$DML_iteration <- c(Results_Grid$DML_iteration, i)
    Results_Grid$DML_fold_seed <- c(Results_Grid$DML_fold_seed, seed_h2o)
    Results_Grid$target_name  <- c(Results_Grid$target_name, target)
    Results_Grid$method_name  <- c(Results_Grid$method_name, method)
    Results_Grid$train_MSE  <- c(Results_Grid$train_MSE, best_DL_perf_train@metrics["MSE"][[1]])
    Results_Grid$xval_MSE <- c(Results_Grid$xval_MSE, best_DL_perf_xval@metrics["MSE"][[1]])
    Results_Grid$GLM_alpha  <- c(Results_Grid$GLM_alpha, NA)
    Results_Grid$GLM_lambda  <- c(Results_Grid$GLM_lambda, NA)
    Results_Grid$DRF_ntrees  <- c(Results_Grid$DRF_ntrees, NA)
    Results_Grid$DRF_max_depth   <- c(Results_Grid$DRF_max_depth, NA)
    Results_Grid$DRF_min_rows   <- c(Results_Grid$DRF_min_rows, NA)
    Results_Grid$DRF_nbins <- c(Results_Grid$DRF_nbins, NA)
    Results_Grid$GBM_learn_rate  <- c(Results_Grid$GBM_learn_rate, NA)
    Results_Grid$GBM_ntrees  <- c(Results_Grid$GBM_ntrees, NA)
    Results_Grid$GBM_max_depth  <- c(Results_Grid$GBM_max_depth, NA)
    Results_Grid$GBM_min_rows  <- c(Results_Grid$GBM_min_rows, NA)
    Results_Grid$GBM_nbins  <- c(Results_Grid$GBM_nbins, NA)
    Results_Grid$DL_hidden  <- c(Results_Grid$DL_hidden, paste(best_DL@allparameters$hidden, collapse = "_"))
    Results_Grid$DL_activation  <- c(Results_Grid$DL_activation, best_DL@allparameters$activation)
    Results_Grid$DL_l1  <- c(Results_Grid$DL_l1, best_DL@allparameters$l1)
    Results_Grid$DL_l2 <- c(Results_Grid$DL_l2, best_DL@allparameters$l2) 
    Results_Grid$grid_seed <- c(Results_Grid$grid_seed, grid_seed_DL)  
  }
  cat('* Remove DL objects that are not needed ... ' , '\n')
  {
    h2o.rm("grid_DL"); rm(grid_DL);rm(grid_DL_perf);
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "grid_DL"));
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "DL_model_R"));
    h2o.rm(best_DL); rm(best_DL_perf_train); rm(best_DL_perf_xval);
    print(h2o.ls())
  }   
}

