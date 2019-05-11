cat('* Initialize a list to store performance of best model ... ', '\n')
{
  Grid_Results = list(DML_iteration = c(),
                      DML_fold_seed = c(),
                      target_name = c(), 
                      method_name = c(), 
                      train_MSE = c(), 
                      xval_MSE = c(),
                      GLM_alpha = c(), GLM_lambda = c(), 
                      DRF_ntrees = c(), DRF_max_depth  = c(), DRF_min_rows  = c(), DRF_nbins = c(),
                      GBM_learn_rate = c(), GBM_ntrees = c(), GBM_max_depth = c(), GBM_min_rows = c(), GBM_nbins = c(), 
                      DL_hidden = c(), DL_activation = c(), DL_l1 = c(), DL_l2 = c(), grid_seed = c()
  )  
}
cat('* Creating a split for this DML process ...', '\n')
{
  seed_h2o = sample(1:999999, 1, replace=TRUE)
  fold_numbers <- h2o.kfold_column(final.df.h2o, nfolds=5,seed = seed_h2o)
  names(fold_numbers) <- "fold_numbers"
  print(fold_numbers)
  final.df.h2o.colnames <- h2o.colnames(final.df.h2o)
  final.df.h2o <- h2o.cbind(final.df.h2o,fold_numbers)
}

cat('* Train the grid on datause for each target', '\n')
for(target in target_list){
  # target = "lprice_per_person"
  # target = "prod_week1_0"
  
  cat('* Check to see if ',target, 'is a BINARY variable ... ',  '\n')
  if(h2o.isfactor(final.df.h2o[,target])){
    fam = "binomial"
    cat('*', target, 'is a BINARY variable.' , '\n')
  } else {
    fam = "gaussian"
    cat('*', target, 'is a CONTINIOUS variable.' , '\n')
  }
  
  cat('* Formulate GLM grid for target : ', target, '.','\n')
  {
    method = "GLM"
    seed = sample(1:999999, 1, replace=TRUE)
    grid_GLM <- h2o.grid(algorithm = "glm", 
                         hyper_params = hparams_GLM, 
                         search_criteria = search_criteria,
                         grid_id = "grid_GLM" , 
                         x = predictor_list, 
                         y = target, 
                         training_frame = final.df.h2o, 
                         fold_column="fold_numbers",
                         seed = -1,
                         standardize = TRUE, 
                         lambda_search = FALSE,
                         family = fam,
                         interaction_pairs = list(c("dayOfWeek", "nbhd")), 
                         interactions = c("year", "week", "nbhd"))
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
    Grid_Results$DML_iteration <- c(Grid_Results$DML_iteration, i)
    Grid_Results$DML_fold_seed <- c(Grid_Results$DML_fold_seed, seed_h2o)
    Grid_Results$target_name  <- c(Grid_Results$target_name, target)
    Grid_Results$method_name  <- c(Grid_Results$method_name, method)
    Grid_Results$train_MSE  <- c(Grid_Results$train_MSE, best_GLM_perf_train@metrics["MSE"][[1]])
    Grid_Results$xval_MSE <- c(Grid_Results$xval_MSE, best_GLM_perf_xval@metrics["MSE"][[1]])
    Grid_Results$GLM_alpha  <- c(Grid_Results$GLM_alpha, best_GLM@allparameters$alpha)
    Grid_Results$GLM_lambda  <- c(Grid_Results$GLM_lambda, best_GLM@allparameters$lambda)
    Grid_Results$DRF_ntrees  <- c(Grid_Results$DRF_ntrees, NA)
    Grid_Results$DRF_max_depth   <- c(Grid_Results$DRF_max_depth, NA)
    Grid_Results$DRF_min_rows   <- c(Grid_Results$DRF_min_rows, NA)
    Grid_Results$DRF_nbins <- c(Grid_Results$DRF_nbins, NA)
    Grid_Results$GBM_learn_rate  <- c(Grid_Results$GBM_learn_rate, NA)
    Grid_Results$GBM_ntrees  <- c(Grid_Results$GBM_ntrees, NA)
    Grid_Results$GBM_max_depth  <- c(Grid_Results$GBM_max_depth, NA)
    Grid_Results$GBM_min_rows  <- c(Grid_Results$GBM_min_rows, NA)
    Grid_Results$GBM_nbins  <- c(Grid_Results$GBM_nbins, NA)
    Grid_Results$DL_hidden  <- c(Grid_Results$DL_hidden, "")
    Grid_Results$DL_activation  <- c(Grid_Results$DL_activation, NA)
    Grid_Results$DL_l1  <- c(Grid_Results$DL_l1, NA)
    Grid_Results$DL_l2 <- c(Grid_Results$DL_l2, NA) 
    Grid_Results$grid_seed <- c(Grid_Results$grid_seed, seed)  
  }
  cat('* Remove GLM objects that are not needed ... ' , '\n')
  {
    h2o.rm("grid_GLM"); rm(grid_GLM);rm(grid_GLM_perf);
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "grid_GLM"));        
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "GLM_model_R"));
    h2o.rm(best_GLM); rm(best_GLM_perf_train); rm(best_GLM_perf_xval);
    print(h2o.ls())
  }
  cat('* Formulate DRF grid for target : ', target, '.','\n')
  {
    method = "DRF"
    seed = sample(1:999999, 1, replace=TRUE)
    grid_DRF <- h2o.grid(algorithm = "randomForest", 
                         hyper_params = hparams_DRF, 
                         search_criteria = search_criteria,
                         grid_id = "grid_DRF" , 
                         x = predictor_list, 
                         y = target, 
                         training_frame = final.df.h2o, 
                         fold_column="fold_numbers",
                         seed = -1,
                         stopping_rounds = 5,
                         stopping_metric = "AUTO", 
                         stopping_tolerance = 0.001,
                         min_split_improvement = 1e-05
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
    Grid_Results$DML_iteration <- c(Grid_Results$DML_iteration, i)
    Grid_Results$DML_fold_seed <- c(Grid_Results$DML_fold_seed, seed_h2o)
    Grid_Results$target_name  <- c(Grid_Results$target_name, target)
    Grid_Results$method_name  <- c(Grid_Results$method_name, method)
    Grid_Results$train_MSE  <- c(Grid_Results$train_MSE, best_DRF_perf_train@metrics["MSE"][[1]])
    Grid_Results$xval_MSE <- c(Grid_Results$xval_MSE, best_DRF_perf_xval@metrics["MSE"][[1]])
    Grid_Results$GLM_alpha  <- c(Grid_Results$GLM_alpha, NA)
    Grid_Results$GLM_lambda  <- c(Grid_Results$GLM_lambda, NA)
    Grid_Results$DRF_ntrees  <- c(Grid_Results$DRF_ntrees, best_DRF@allparameters$ntrees)
    Grid_Results$DRF_max_depth   <- c(Grid_Results$DRF_max_depth, best_DRF@allparameters$max_depth)
    Grid_Results$DRF_min_rows   <- c(Grid_Results$DRF_min_rows, best_DRF@allparameters$min_rows)
    Grid_Results$DRF_nbins <- c(Grid_Results$DRF_nbins, best_DRF@allparameters$nbins)
    Grid_Results$GBM_learn_rate  <- c(Grid_Results$GBM_learn_rate, NA)
    Grid_Results$GBM_ntrees  <- c(Grid_Results$GBM_ntrees, NA)
    Grid_Results$GBM_max_depth  <- c(Grid_Results$GBM_max_depth, NA)
    Grid_Results$GBM_min_rows  <- c(Grid_Results$GBM_min_rows, NA)
    Grid_Results$GBM_nbins  <- c(Grid_Results$GBM_nbins, NA)
    Grid_Results$DL_hidden  <- c(Grid_Results$DL_hidden, "")
    Grid_Results$DL_activation  <- c(Grid_Results$DL_activation, NA)
    Grid_Results$DL_l1  <- c(Grid_Results$DL_l1, NA)
    Grid_Results$DL_l2 <- c(Grid_Results$DL_l2, NA) 
    Grid_Results$grid_seed <- c(Grid_Results$grid_seed, seed)  
  }
  cat('* Remove DRF objects that are not needed ... ' , '\n')
  {
    h2o.rm("grid_DRF"); rm(grid_DRF);rm(grid_DRF_perf);
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "grid_DRF"));
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "DRF_model_R"));
    h2o.rm(best_DRF); rm(best_DRF_perf_train); rm(best_DRF_perf_xval);
    print(h2o.ls())
  }
  cat('* Formulate GBM grid for target : ', target, '.','\n')
  {
    method = "GBM"
    seed = sample(1:999999, 1, replace=TRUE)
    grid_GBM <- h2o.grid(algorithm = "gbm", 
                         hyper_params = hparams_GBM, 
                         search_criteria = search_criteria,
                         grid_id = "grid_GBM" , 
                         x = predictor_list, 
                         y = target, 
                         training_frame = final.df.h2o, 
                         fold_column="fold_numbers", 
                         seed = -1,
                         learn_rate_annealing = 1, 
                         stopping_rounds = 5, 
                         stopping_metric = "AUTO", 
                         stopping_tolerance = 0.001, 
                         min_split_improvement = 1e-05)
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
    Grid_Results$DML_iteration <- c(Grid_Results$DML_iteration, i)
    Grid_Results$DML_fold_seed <- c(Grid_Results$DML_fold_seed, seed_h2o)
    Grid_Results$target_name  <- c(Grid_Results$target_name, target)
    Grid_Results$method_name  <- c(Grid_Results$method_name, method)
    Grid_Results$train_MSE  <- c(Grid_Results$train_MSE, best_GBM_perf_train@metrics["MSE"][[1]])
    Grid_Results$xval_MSE <- c(Grid_Results$xval_MSE, best_GBM_perf_xval@metrics["MSE"][[1]])
    Grid_Results$GLM_alpha  <- c(Grid_Results$GLM_alpha, NA)
    Grid_Results$GLM_lambda  <- c(Grid_Results$GLM_lambda, NA)
    Grid_Results$DRF_ntrees  <- c(Grid_Results$DRF_ntrees, NA)
    Grid_Results$DRF_max_depth   <- c(Grid_Results$DRF_max_depth, NA)
    Grid_Results$DRF_min_rows   <- c(Grid_Results$DRF_min_rows, NA)
    Grid_Results$DRF_nbins <- c(Grid_Results$DRF_nbins, NA)
    Grid_Results$GBM_learn_rate  <- c(Grid_Results$GBM_learn_rate, best_GBM@allparameters$learn_rate)
    Grid_Results$GBM_ntrees  <- c(Grid_Results$GBM_ntrees, best_GBM@allparameters$ntrees)
    Grid_Results$GBM_max_depth  <- c(Grid_Results$GBM_max_depth, best_GBM@allparameters$max_depth)
    Grid_Results$GBM_min_rows  <- c(Grid_Results$GBM_min_rows, best_GBM@allparameters$min_rows)
    Grid_Results$GBM_nbins  <- c(Grid_Results$GBM_nbins, best_GBM@allparameters$nbins)
    Grid_Results$DL_hidden  <- c(Grid_Results$DL_hidden, "")
    Grid_Results$DL_activation  <- c(Grid_Results$DL_activation, NA)
    Grid_Results$DL_l1  <- c(Grid_Results$DL_l1, NA)
    Grid_Results$DL_l2 <- c(Grid_Results$DL_l2, NA) 
    Grid_Results$grid_seed <- c(Grid_Results$grid_seed, seed)  
  }
  cat('* Remove GBM objects that are not needed ... ' , '\n')
  {
    h2o.rm("grid_GBM"); rm(grid_GBM);rm(grid_GBM_perf);
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "grid_GBM"));
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "GBM_model_R"));
    h2o.rm(best_GBM); rm(best_GBM_perf_train); rm(best_GBM_perf_xval);
    print(h2o.ls())
  }
  cat('* Formulate DL grid for target : ', target, '.','\n')
  {
    method = "DL"
    seed = sample(1:999999, 1, replace=TRUE)
    grid_DL <- h2o.grid(algorithm = "deeplearning", 
                        hyper_params = hparams_DL, 
                        search_criteria = search_criteria,
                        grid_id = "grid_DL" , 
                        x = predictor_list, 
                        y = target, 
                        training_frame = final.df.h2o, 
                        fold_column="fold_numbers",
                        seed = -1,
                        epochs = 10, 
                        distribution = c("AUTO"), 
                        score_interval = 5,
                        score_training_samples = 10000, 
                        score_validation_samples = 0,
                        score_duty_cycle = 0.1, 
                        classification_stop = 0,
                        regression_stop = 1e-06,
                        stopping_rounds = 5,
                        stopping_metric = c("AUTO"),
                        stopping_tolerance = 0.001, 
                        max_runtime_secs = 0, 
                        sparse = FALSE)
    
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
    Grid_Results$DML_iteration <- c(Grid_Results$DML_iteration, i)
    Grid_Results$DML_fold_seed <- c(Grid_Results$DML_fold_seed, seed_h2o)
    Grid_Results$target_name  <- c(Grid_Results$target_name, target)
    Grid_Results$method_name  <- c(Grid_Results$method_name, method)
    Grid_Results$train_MSE  <- c(Grid_Results$train_MSE, best_DL_perf_train@metrics["MSE"][[1]])
    Grid_Results$xval_MSE <- c(Grid_Results$xval_MSE, best_DL_perf_xval@metrics["MSE"][[1]])
    Grid_Results$GLM_alpha  <- c(Grid_Results$GLM_alpha, NA)
    Grid_Results$GLM_lambda  <- c(Grid_Results$GLM_lambda, NA)
    Grid_Results$DRF_ntrees  <- c(Grid_Results$DRF_ntrees, NA)
    Grid_Results$DRF_max_depth   <- c(Grid_Results$DRF_max_depth, NA)
    Grid_Results$DRF_min_rows   <- c(Grid_Results$DRF_min_rows, NA)
    Grid_Results$DRF_nbins <- c(Grid_Results$DRF_nbins, NA)
    Grid_Results$GBM_learn_rate  <- c(Grid_Results$GBM_learn_rate, NA)
    Grid_Results$GBM_ntrees  <- c(Grid_Results$GBM_ntrees, NA)
    Grid_Results$GBM_max_depth  <- c(Grid_Results$GBM_max_depth, NA)
    Grid_Results$GBM_min_rows  <- c(Grid_Results$GBM_min_rows, NA)
    Grid_Results$GBM_nbins  <- c(Grid_Results$GBM_nbins, NA)
    Grid_Results$DL_hidden  <- c(Grid_Results$DL_hidden, paste(best_DL@allparameters$hidden, collapse = "_"))
    Grid_Results$DL_activation  <- c(Grid_Results$DL_activation, best_DL@allparameters$activation)
    Grid_Results$DL_l1  <- c(Grid_Results$DL_l1, best_DL@allparameters$l1)
    Grid_Results$DL_l2 <- c(Grid_Results$DL_l2, best_DL@allparameters$l2) 
    Grid_Results$grid_seed <- c(Grid_Results$grid_seed, seed)  
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

cat('* Output Grid search and DML process - ', i , ' - time.','\n')
{
  cat('* Model Performance data frame ... ', '\n')
  Grid_Results <- Grid_Results %>% as.data.frame()
  
  cat('* Write All grid results on a file ... ', '\n')
  {
    Grid_Results <- Grid_Results %>% setDT()
    if(i==1){
      fwrite(Grid_Results, file =  paste(project.path, "Output/Final/", "50_00_Grid_Results", ".csv", sep = ""), append = FALSE)
    }else{
      fwrite(Grid_Results, file =  paste(project.path, "Output/Final/", "50_00_Grid_Results", ".csv", sep = ""), append = TRUE)
    }  
  }
}

cat('* Remove/Restore object created in Grid search and DML process - ', i , ' - time.','\n')
{
  # rm(seed_h2o); 
  final.df.h2o <- final.df.h2o[, final.df.h2o.colnames]
  print(h2o.ls())
}