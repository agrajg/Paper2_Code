# NOTES : 
cat('* Begin Running file : ', '50_00_DML_test.R ', " ...", '\n')
cat('* --------------------------------------------------', '\n')
cat('* R Setup ... ', '\n')
source(file = '50_00_R_Setup.R')

cat('* Feeding Inputs ... ', '\n')
{
  cat('* H2O port ... ', '\n')
  {  
    n.threads = 66
    port.num = 11112
    max.mem = '550G'
    force.DL = TRUE
  }
  
  {
    max_text_features = 10000
  }
}

cat('* Setting up data ...', '\n')
source(file = "50_00_Setting_up_data.R")
h2o.ls()

cat('* Feeding Inputs ... ','\n')
{
  cat('* Setting R seed ', '\n')
  {
    Seed_R = 9238928
    cat('* Seed = ', Seed_R, '\n')
    set.seed(Seed_R)
  }
  
  cat('* For DML algorithm ...', '\n')     
  {
    times.split.dml = 1                           # *** Change this in the final program
    crossfit.K = 2                                # *** Change this in the final program
  }
  
  cat('* Model Selection hyper parameters ... ', '\n')
  {
    # need alpha and lambda
    hparams_GLM = list(alpha = c(0.01, 0.1, 0.5, 0.9, 1), lambda = c(0.0001, 0.001, 0.01, 0.1, 1, 10))   # *** Change this in the final program
    # need number of trees, max depth - how many variables to break on. 
    # https://medium.com/all-things-ai/in-depth-parameter-tuning-for-random-forest-d67bb7e920d
    hparams_DRF = list(ntrees = c(50, 100, 500), 
                       max_depth = c(20, 40 , 80), 
                       min_rows = c(100, 1000, 2000),
                       nbins = c(20, 40, 80))    
    hparams_GBM = list(learn_rate = c(0.1, 0.3, 0.7),
                       ntrees = c(50, 100, 500), 
                       max_depth = c(20, 40 , 80), 
                       min_rows = c(100, 1000, 2000),
                       nbins = c(20, 40, 80))    
    # https://htmlpreview.github.io/?https://github.com/ledell/sldm4-h2o/blob/master/sldm4-deeplearning-h2o.html
    hparams_DL = list(hidden = list(c(10,20,10), c(10, 20, 10), c(50,20)), 
                      activation = c("Rectifier", "Maxout", "Tanh"), 
                      l1 = c(0, 0.00001, 0.0001, 0.001, 0.01), 
                      l2 = c(0, 0.00001, 0.0001, 0.001, 0.01))
  }
  
  cat('* For ML algorithms ...', '\n')
  {
    method_list = c("GLM", "DRF", "GBM", "DL")
    # validation.split.frac = (4/5)                     # ***Either do validation or cross validation
    cv.num.folds = 5                                    # *** Change this in the final program
    search_criteria <- list(strategy = "RandomDiscrete", max_models = 2, stopping_tolerance = 0.0001)
  }

}

cat('* MAIN PROGRAM ... ', '\n')
{
  cat('* Define predictors for all outcomes ... ')
  {
    predictor_list = X                # Change this in final program
    cat('* Define target list ... ', '\n')
    target_list = c(Y[1], D[1], Z[1:2])       # Change this in final program
  }
  
  cat('* Model Selection Proocess and DML ... ', '\n')
  for(i in 1:times.split.dml){
    #  i=1
    cat('* Running Grid search and DML process - ', i , ' - time.','\n')
    cat('* Creating a split for this DML process ...', '\n')
    {
      seed_h2o = sample(1:999999, 1, replace=TRUE)
      split     <- h2o.runif(x = final.df.h2o, seed = seed_h2o)
      cvgroup   <- as.numeric(cut(split,quantile(split,probs = seq(0, 1, 1/crossfit.K)),include.lowest = TRUE)) + 1
    }
    
    cat('* Train the grid on datause for each target', '\n')
    for(target in target_list){
      # target = "lprice_per_person"
      # target = "prod_week1_0"
      cat('* Formulate GLM grid for target : ', target, '.','\n')
      {
        grid_GLM <- h2o.grid(algorithm = "glm", 
                             hyper_params = hparams_GLM, 
                             search_criteria = search_criteria,
                             grid_id = "grid_GLM" , 
                             x = predictor_list, 
                             y = target, 
                             training_frame = datause_train, 
                             validation_frame = datause_valid, 
                             nfolds = cv.num.folds, 
                             standardize = TRUE, 
                             lambda_search = FALSE,
                             family = fam,
                             interaction_pairs = list(c("dayOfWeek", "nbhd")), 
                             interactions = c("year", "week", "nbhd"))
        cat('* Evaluating GLM grid performance ... ')
        grid_GLM_perf <- h2o.getGrid(grid_id = "grid_GLM", sort_by = evalmetric , decreasing = dec)
        cat('* Grid GLM models : ')
        print(grid_GLM_perf)
        
        cat('* Picking up the best GLM model ... ',  '\n')
        best_GLM <- h2o.getModel(grid_GLM_perf@model_ids[[1]])
        cat('* Calculating best GLM performance on train valid and test(dataout)... ', '\n')
        best_GLM_perf_train <- h2o.performance(best_GLM, train = TRUE)
        best_GLM_perf_valid <- h2o.performance(best_GLM, valid = TRUE)
        best_GLM_perf_test <- h2o.performance(best_GLM, newdata = dataout)
      }
      
    }
    
    
    cat('* Output Grid search and DML process - ', i , ' - time.','\n')
    {
      
    }
    cat('* Remove object created in Grid search and DML process - ', i , ' - time.','\n')
    {
      # rm(seed_h2o); h2o.rm(split); h2o.rm(cvgroup)
      h2o.ls()
    }
  }
}

{
  
    # i=1
    cat('* Initialize a list to store performance of best model ... ', '\n')
    {
      Grid_Results = list(DML_iteration = c(),
                          CF_group = c(),
                          target_name = c(), 
                          method_name = c(), 
                          eval_metric_name = c(), 
                          train_evalmetric_value = c(), 
                          valid_evalmetric_value = c(),
                          test_evalmetric_value = c(), 
                          GLM_alpha = c(), GLM_lambda = c(), 
                          DRF_ntrees = c(), DRF_max_depth  = c(), DRF_min_rows  = c(), DRF_nbins = c(),
                          GBM_learn_rate = c(), GBM_ntrees = c(), GBM_max_depth = c(), GBM_min_rows = c(), GBM_nbins = c(), 
                          DL_hidden = c(), DL_activation = c(), DL_l1 = c(), DL_l2 = c(), grid_seed = c()
      )  
    }
    
    
    cat('* Using datause to train a grid and find best model ... ', '\n')
    for(j in 1:crossfit.K){
      # j=1
      cat('* Dataout sample (test) sample count - ', j, ' .','\n')
      cat('* Assigning datause and dataout ... ','\n')
      {
        ii  <- cvgroup == j
        nii <- cvgroup != j
        if(crossfit.K==1){
          ii  <- cvgroup == j
          nii <- cvgroup == j
        }
        datause <- final.df.h2o[nii,]
        dataout <- final.df.h2o[ii,]
      }
      cat('* Partitioning the DML train data into grid train and validation data ...', '\n')
      {
        datause.split <- h2o.splitFrame(data=datause, ratios=0.75)
        datause_train <- datause.split[[1]]
        datause_valid <- datause.split[[2]]
      }
      
      
        cat('* The target varible is =', target, '.' , '\n')
        cat('* Check and change parmeters if the target is binary', '\n')
        if(h2o.isfactor(final.df.h2o[,target])){
          fam = "binomial"
          evalmetric = "AUC" 
          dec = TRUE
          cat('*', target, 'is a BINARY variable.' , '\n')
        } else {
          fam = "gaussian"
          evalmetric = "MSE"
          dec = FALSE
          cat('*', target, 'is a CONTINIOUS variable.' , '\n')
        }
        
        for(method in method_list){
          # method = "GLM"
          cat('* The ML method is =', method, '.' , '\n')
          cat('* Train the grid ... ', '\n')
          
          if(method == "GLM"){
            print(h2o.ls())
            cat('* Formulate GLM grid for target : ', target, '.','\n')
            {
              seed = sample(1:999999, 1, replace=TRUE)
              grid_GLM <- h2o.grid(algorithm = "glm", 
                                   hyper_params = hparams_GLM, 
                                   search_criteria = search_criteria,
                                   grid_id = "grid_GLM" , 
                                   x = predictor_list, 
                                   y = target, 
                                   training_frame = datause_train, 
                                   validation_frame = datause_valid, 
                                   nfolds = cv.num.folds, 
                                   standardize = TRUE, 
                                   lambda_search = FALSE,
                                   family = fam,
                                   interaction_pairs = list(c("dayOfWeek", "nbhd")), 
                                   interactions = c("year", "week", "nbhd"))
              cat('* Evaluating GLM grid performance ... ')
              grid_GLM_perf <- h2o.getGrid(grid_id = "grid_GLM", sort_by = evalmetric , decreasing = dec)
              cat('* Grid GLM models : ')
              print(grid_GLM_perf)
              
              cat('* Picking up the best GLM model ... ',  '\n')
              best_GLM <- h2o.getModel(grid_GLM_perf@model_ids[[1]])
              cat('* Calculating best GLM performance on train valid and test(dataout)... ', '\n')
              best_GLM_perf_train <- h2o.performance(best_GLM, train = TRUE)
              best_GLM_perf_valid <- h2o.performance(best_GLM, valid = TRUE)
              best_GLM_perf_test <- h2o.performance(best_GLM, newdata = dataout)
            }
            cat('* Writing the GLM performance list ... ', '\n')
            {
              Grid_Results$DML_iteration <- c(Grid_Results$DML_iteration, i)
              Grid_Results$CF_group <- c(Grid_Results$CF_group, j)
              Grid_Results$target_name  <- c(Grid_Results$target_name, target)
              Grid_Results$method_name  <- c(Grid_Results$method_name, method)
              Grid_Results$eval_metric_name  <- c(Grid_Results$eval_metric_name, evalmetric)
              Grid_Results$train_evalmetric_value  <- c(Grid_Results$train_evalmetric_value, best_GLM_perf_train@metrics[evalmetric][[1]])
              Grid_Results$valid_evalmetric_value <- c(Grid_Results$valid_evalmetric_value, best_GLM_perf_valid@metrics[evalmetric][[1]])
              Grid_Results$test_evalmetric_value  <- c(Grid_Results$test_evalmetric_value, best_GLM_perf_test@metrics[evalmetric][[1]])
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
              h2o.rm(best_GLM); rm(best_GLM_perf_train); rm(best_GLM_perf_valid); rm(best_GLM_perf_test);
              print(h2o.ls())
            }
          }
          
          if(method == "DRF"){
            print(h2o.ls())
            cat('* Formulate DRF grid for target : ', target, '.','\n')
            {
              seed = sample(1:999999, 1, replace=TRUE)
              grid_DRF <- h2o.grid(algorithm = "randomForest", 
                                   hyper_params = hparams_DRF, 
                                   search_criteria = search_criteria,
                                   grid_id = "grid_DRF" , 
                                   x = predictor_list, 
                                   y = target, 
                                   training_frame = datause_train, 
                                   validation_frame = datause_valid, 
                                   nfolds = cv.num.folds, 
                                   stopping_rounds = 5,
                                   stopping_metric = "AUTO", 
                                   stopping_tolerance = 0.001,
                                   seed = seed,
                                   min_split_improvement = 1e-05
              )
              cat('* Evaluating DRF grid performance ... ')
              grid_DRF_perf <- h2o.getGrid(grid_id = "grid_DRF", sort_by = evalmetric , decreasing = dec)
              cat('* Grid DRF models : ')
              print(grid_DRF_perf)
              
              cat('* Picking up the best DRF model ... ',  '\n')
              best_DRF <- h2o.getModel(grid_DRF_perf@model_ids[[1]])
              cat('* Calculating best DRF performance on train valid and test(dataout)... ', '\n')
              best_DRF_perf_train <- h2o.performance(best_DRF, train = TRUE)
              best_DRF_perf_valid <- h2o.performance(best_DRF, valid = TRUE)
              best_DRF_perf_test <- h2o.performance(best_DRF, newdata = dataout)
            }
            cat('* Writing the DRF performance list ... ', '\n')
            {
              Grid_Results$DML_iteration <- c(Grid_Results$DML_iteration, i)
              Grid_Results$CF_group <- c(Grid_Results$CF_group, j)
              Grid_Results$target_name  <- c(Grid_Results$target_name, target)
              Grid_Results$method_name  <- c(Grid_Results$method_name, method)
              Grid_Results$eval_metric_name  <- c(Grid_Results$eval_metric_name, evalmetric)
              Grid_Results$train_evalmetric_value  <- c(Grid_Results$train_evalmetric_value, best_DRF_perf_train@metrics[evalmetric][[1]])
              Grid_Results$valid_evalmetric_value <- c(Grid_Results$valid_evalmetric_value, best_DRF_perf_valid@metrics[evalmetric][[1]])
              Grid_Results$test_evalmetric_value  <- c(Grid_Results$test_evalmetric_value, best_DRF_perf_test@metrics[evalmetric][[1]])
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
              h2o.rm(best_DRF); rm(best_DRF_perf_train); rm(best_DRF_perf_valid); rm(best_DRF_perf_test);
              print(h2o.ls())
            }
          }
          
          if(method == "GBM"){
            print(h2o.ls())
            cat('* Formulate GBM grid for target : ', target, '.','\n')
            {
              seed = sample(1:999999, 1, replace=TRUE)
              grid_GBM <- h2o.grid(algorithm = "gbm", 
                                   hyper_params = hparams_GBM, 
                                   search_criteria = search_criteria,
                                   grid_id = "grid_GBM" , 
                                   x = predictor_list, 
                                   y = target, 
                                   training_frame = datause_train, 
                                   validation_frame = datause_valid, 
                                   nfolds = cv.num.folds, 
                                   learn_rate_annealing = 1, 
                                   stopping_rounds = 5, 
                                   stopping_metric = "AUTO", 
                                   stopping_tolerance = 0.001, 
                                   min_split_improvement = 1e-05)
              cat('* Evaluating GBM grid performance ... ')
              grid_GBM_perf <- h2o.getGrid(grid_id = "grid_GBM", sort_by = evalmetric , decreasing = dec)
              cat('* Grid GBM models : ')
              print(grid_GBM_perf)
              
              cat('* Picking up the best GBM model ... ',  '\n')
              best_GBM <- h2o.getModel(grid_GBM_perf@model_ids[[1]])
              cat('* Calculating best GBM performance on train valid and test(dataout)... ', '\n')
              best_GBM_perf_train <- h2o.performance(best_GBM, train = TRUE)
              best_GBM_perf_valid <- h2o.performance(best_GBM, valid = TRUE)
              best_GBM_perf_test <- h2o.performance(best_GBM, newdata = dataout)
            }
            cat('* Writing the GBM performance list ... ', '\n')
            {
              Grid_Results$DML_iteration <- c(Grid_Results$DML_iteration, i)
              Grid_Results$CF_group <- c(Grid_Results$CF_group, j)
              Grid_Results$target_name  <- c(Grid_Results$target_name, target)
              Grid_Results$method_name  <- c(Grid_Results$method_name, method)
              Grid_Results$eval_metric_name  <- c(Grid_Results$eval_metric_name, evalmetric)
              Grid_Results$train_evalmetric_value  <- c(Grid_Results$train_evalmetric_value, best_GBM_perf_train@metrics[evalmetric][[1]])
              Grid_Results$valid_evalmetric_value <- c(Grid_Results$valid_evalmetric_value, best_GBM_perf_valid@metrics[evalmetric][[1]])
              Grid_Results$test_evalmetric_value  <- c(Grid_Results$test_evalmetric_value, best_GBM_perf_test@metrics[evalmetric][[1]])
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
              h2o.rm(best_GBM); rm(best_GBM_perf_train); rm(best_GBM_perf_valid); rm(best_GBM_perf_test);
              print(h2o.ls())
            }
          }
          
          if(method == "DL"){
            print(h2o.ls())
            cat('* Formulate DL grid for target : ', target, '.','\n')
            {
              seed = sample(1:999999, 1, replace=TRUE)
              grid_DL <- h2o.grid(algorithm = "deeplearning", 
                                  hyper_params = hparams_DL, 
                                  search_criteria = search_criteria,
                                  grid_id = "grid_DL" , 
                                  x = predictor_list, 
                                  y = target, 
                                  training_frame = datause_train, 
                                  validation_frame = datause_valid, 
                                  nfolds = cv.num.folds, 
                                  epochs = 10, seed = seed, 
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
              grid_DL_perf <- h2o.getGrid(grid_id = "grid_DL", sort_by = evalmetric , decreasing = dec)
              cat('* Grid DL models : ')
              print(grid_DL_perf)
              
              cat('* Picking up the best DL model ... ',  '\n')
              best_DL <- h2o.getModel(grid_DL_perf@model_ids[[1]])
              cat('* Calculating best GLM performance on train valid and test(dataout)... ', '\n')
              best_DL_perf_train <- h2o.performance(best_DL, train = TRUE)
              best_DL_perf_valid <- h2o.performance(best_DL, valid = TRUE)
              best_DL_perf_test <- h2o.performance(best_DL, newdata = dataout)
            }
            cat('* Writing the DL performance list ... ', '\n')
            {
              Grid_Results$DML_iteration <- c(Grid_Results$DML_iteration, i)
              Grid_Results$CF_group <- c(Grid_Results$CF_group, j)
              Grid_Results$target_name  <- c(Grid_Results$target_name, target)
              Grid_Results$method_name  <- c(Grid_Results$method_name, method)
              Grid_Results$eval_metric_name  <- c(Grid_Results$eval_metric_name, evalmetric)
              Grid_Results$train_evalmetric_value  <- c(Grid_Results$train_evalmetric_value, best_DL_perf_train@metrics[evalmetric][[1]])
              Grid_Results$valid_evalmetric_value <- c(Grid_Results$valid_evalmetric_value, best_DL_perf_valid@metrics[evalmetric][[1]])
              Grid_Results$test_evalmetric_value  <- c(Grid_Results$test_evalmetric_value, best_DL_perf_test@metrics[evalmetric][[1]])
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
              h2o.rm(best_DL); rm(best_DL_perf_train); rm(best_DL_perf_valid); rm(best_DL_perf_test);
              print(h2o.ls())
            }
          }
        }
      }
    }
    
    
    cat('* Gather information on model performance', '\n')
    {
      cat('* Model Performance data frame', '\n')
      Grid_Results <- Grid_Results %>% as.data.frame()
      
      cat('* Define best models based on highest AUC and lowest MSE', '\n')
      {
        Grid_Results <- Grid_Results %>%  as.data.frame() %>% mutate(order_metrics = if_else(eval_metric_name == "MSE",test_evalmetric_value,-test_evalmetric_value))
        Best_Model_Within_Method <- Grid_Results %>% group_by(DML_iteration, target_name, method_name) %>% slice(which.min(order_metrics))
        Best_Model_All_Methods <- Grid_Results %>% group_by(DML_iteration, target_name) %>% slice(which.min(order_metrics))
      }
      
      cat('* Write All grid results on a file ... ', '\n')
      {
        Grid_Results <- Grid_Results %>% setDT()
        if(i==1){
          fwrite(Grid_Results, file =  paste(project.path, "Output/Final/", "50_00_Grid_Results", ".csv", sep = ""), append = FALSE)
          fwrite(Best_Model_Within_Method, file =  paste(project.path, "Output/Final/", "50_00_Best_Model_Within_Method", ".csv", sep = ""), append = FALSE)
          fwrite(Best_Model_All_Methods, file =  paste(project.path, "Output/Final/", "50_00_Best_Model_All_Methods", ".csv", sep = ""), append = FALSE)
        }else{
          fwrite(Grid_Results, file =  paste(project.path, "Output/Final/", "50_00_Grid_Results", ".csv", sep = ""), append = TRUE)
          fwrite(Best_Model_Within_Method, file =  paste(project.path, "Output/Final/", "50_00_Best_Model_Within_Method", ".csv", sep = ""), append = TRUE)
          fwrite(Best_Model_All_Methods, file =  paste(project.path, "Output/Final/", "50_00_Best_Model_All_Methods", ".csv", sep = ""), append = TRUE)
        }  
      }
      
    }
    
    
    h2o.ls()
    cat('* Now running actual DML ... ', '\n')
    
    # Initializing results lists etc.
    stats.name <- c("beta", "var")
    results.list.names <- apply(expand.grid(stats.name, D), 1, paste, collapse="_")
    
    Results_DML1 <- vector("list", length = length(results.list.names) + 2)
    Results_DML2 <- vector("list", length = length(results.list.names) + 2)
    names(Results_DML1) <- c("DML_iteration", "method_name", results.list.names)
    names(Results_DML2) <- c("DML_iteration", "method_name", results.list.names)
    
    
    
    
    cat('* Running DML using GLM ... ', '\n')
    source(file = 'DML_using_GLM.R')
    
    
    
    cat('* Using datause to train best models again and obtain resiuals on dataout ... ', '\n')
    
    
    cat('* ')
    
    
  }
  
  cat('* Using datause to train best models again and obtain resiuals on dataout ... ', '\n')
  for(j in 1:crossfit.K){
    
    
    
    
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
cat('* Removing unnecessary stuff' , '\n')
{
  rm(Grid_Results); rm(Best_Model_Within_Method); rm(Best_Model_All_Methods)
  gc(); gc(); gc();
}
cat('* Ending DML process - ', i , ' - time.', '\n')
}
}
