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
  }
  cat('* Remove GLM objects that are not needed ... ' , '\n')
  {
    h2o.rm("grid_GLM"); rm(grid_GLM);rm(grid_GLM_perf);
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "grid_GLM"));        
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "GLM_model_R"));
    h2o.rm(best_GLM); rm(best_GLM_perf_train); rm(best_GLM_perf_xval);
    print(h2o.ls())
  }
}

