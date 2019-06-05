# NOTES : H20 should be turned on.
cat('* Begin Running file : ', 'DML_Main.R ', " ...", '\n')
cat('* --------------------------------------------', '\n')
cat('* Load_Libraries ...', '\n')
{
  library(slam)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(stringr)
  library(stringi)
  library(gmm)
  library(tidyr)
  library(dplyr)
  library(data.table)
  library(ggplot2)
  library(h2o)
}

cat('* Take all inputs ...', '\n')
{
  filename.begin = "50_00"                                                              # For I/O Setup
  max_text_features = 15000                                                             # For 50_00_Setting_up_data.R
  Seed_R = 9238928                                                                      # Seed for DML Folds
  times.split.dml = 1                                                                   # Number of times to perform DML 
  K.folds = 2                                                                           # Number of folds for CV and crossfitting
  hparams_GLM = list(alpha = c(0.01, 0.1, 0.5, 0.9, 1), 
                     lambda = c(0.0001, 0.001, 0.01, 0.1, 1, 10))                       # *** Change this in the final program
  standardize_param_GLM = TRUE
  lambda_search_param_GLM = FALSE
  interaction_pairs_param_GLM = list(c("dayOfWeek", "nbhd"), c("latitude", "latitude"), c("longitude", "longitude"), c("latitude", "longitude"))
  interactions_param_GLM = c("year", "week", "nbhd")
  
  # DRF Parameters
  hparams_DRF = list(ntrees = c(50, 100, 500), 
                     max_depth = c(20, 40 , 80), 
                     min_rows = c(100, 1000, 2000),
                     nbins = c(20, 40, 80))                                                   # *** Change this in the final program
  stopping_rounds_param_DRF = 5
  stopping_metric_param_DRF = "AUTO"
  stopping_tolerance_param_DRF = 0.001
  min_split_improvement_param_DRF = 1e-05
  
  # GBM Parameters
  hparams_GBM = list(learn_rate = c(0.1, 0.3, 0.7),
                     ntrees = c(50, 100, 500), 
                     max_depth = c(20, 40 , 80), 
                     min_rows = c(100, 1000, 2000),
                     nbins = c(20, 40, 80))                                                   # *** Change this in the final program
  learn_rate_annealing_param_GBM = 1
  stopping_rounds_param_GBM = 5
  stopping_metric_param_GBM = "AUTO"
  stopping_tolerance_param_GBM = 0.001
  min_split_improvement_param_GBM = 1e-05
  
  # Deep learning parameters
  # https://htmlpreview.github.io/?https://github.com/ledell/sldm4-h2o/blob/master/sldm4-deeplearning-h2o.html
  hparams_DL = list(hidden = list(c(10,20,10), c(10, 20, 10), c(50,20)), 
                    activation = c("Rectifier", "Maxout", "Tanh"), 
                    l1 = c(0, 0.00001, 0.0001, 0.001, 0.01), 
                    l2 = c(0, 0.00001, 0.0001, 0.001, 0.01))                                  # *** Change this in the final program  
  epochs_param_DL = 10
  distribution_param_DL = c("AUTO")
  score_interval_param_DL = 5
  score_training_samples_param_DL = 10000
  score_validation_samples_param_DL = 0
  score_duty_cycle_param_DL = 0.1
  classification_stop_param_DL = 0 
  regression_stop_param_DL = 1e-06
  stopping_rounds_param_DL = 5
  stopping_metric_param_DL = c("AUTO")
  stopping_tolerance_param_DL = 0.001
  max_runtime_secs_param_DL = 0
  sparse_param_DL = FALSE
  
  # Other ML inputs
  method_list = c("GLM", "DRF", "GBM", "DL")
  search_criteria <- list(strategy = "RandomDiscrete", max_models = 2, stopping_tolerance = 0.0001)
}

cat('* I/O Setup ...', '\n')
{
  filename.end = gsub(x = format(Sys.time(), "%Y %m %d %H %M %S"), pattern = " ", replacement = "_")
  cat('* Output files begin with : ', filename.begin, '\n')
  cat('* Output files end with : ', filename.end, '\n')
  cat('* FILE : 50_00_Set_project_paths_and_create_folders.R', '\n')
  source(file = '50_00_Set_project_paths_and_create_folders.R')
}

cat('* Importing and preparing the data for DML.', '\n')
{
  cat('- Maximum text features to use = ', max_text_features, '\n')
  cat('* FILE : 50_00_Setting_up_data.R', '\n')
  source(file = "50_00_Setting_up_data.R")
  
  cat('* FILE : 50_00_Defining_variables.R', '\n')
  source(file = '50_00_Defining_variables.R')                                             # Change this program in the final program
}

cat('* FILE : 50_00_DML_Print_All_ML_Parameters.R', '\n')
source(file = '50_00_DML_Print_All_ML_Parameters.R')

cat('* MAIN PROGRAM : Model Selection and DML procedure ... ', '\n')
{
  cat('* Define tragets (Y,D,Z) and predictors for each target (nuisance l(X), r(X), m(X))... ', '\n')
  {
    predictor_list = X                                                                           # Change this in final program
    cat('* Define target list ... ', '\n')
    target_list = c(Y, D, Z)                                                                     # Change this in final program
  }
  
  cat('* Model Selection Proocess and DML ... ', '\n')
  for(i in 1:times.split.dml){
    cat('* Running Grid search and DML process - ', i , ' - time ... ','\n')
    cat('* Creating a split for this DML process, Iteration = ', i , '\n')
    {
      seed_h2o = sample(1:999999, 1, replace=TRUE)
      fold_numbers <- h2o.kfold_column(final_df_h2o, nfolds=K.folds, seed = seed_h2o)
      names(fold_numbers) <- "fold_numbers"
      print(fold_numbers)
      final_df_h2o.colnames <- h2o.colnames(final_df_h2o)
      final_df_h2o <- h2o.cbind(final_df_h2o,fold_numbers)
    }
    
    cat('* Initialize a list to store performance of best model (Results_Grid), Iteration = ', i , '\n')
    {
      Results_Grid = list(DML_iteration = c(),
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
    
    cat('* FILE : 50_00_Model_Selection.R, Iteration = ', i , '\n')
    source(file = '50_00_Model_Selection.R')
    
    cat('* Output Grid search results to CSV, Iteration = ', i , '\n')
    {
      cat('* Model Performance data frame ... ', '\n')
      Results_Grid <- Results_Grid %>% as.data.frame()
      
      cat('* Write All grid results on a file ... ', '\n')
      {
        Results_Grid <- Results_Grid %>% setDT()
        if(i==1){
          fwrite(Results_Grid, file =  paste(project.path, "Output/Final/", paste(filename.begin, "Results_Grid", filename.end, sep = "_"), ".csv", sep = ""), append = FALSE)
        }else{
          fwrite(Results_Grid, file =  paste(project.path, "Output/Final/", paste(filename.begin, "Results_Grid", filename.end, sep = "_"), ".csv", sep = ""), append = TRUE)
        }  
      }
    }
    
    cat('* Actual DML, Iteration = ', i , '\n')
    {
      cat('Initializing ML performance and DML results lists etc., Iteration = ', i , '\n')
      {
        stats.name <- c("beta", "se")
        results.list.names <- apply(expand.grid(stats.name, D), 1, paste, collapse=".")
        Results_DML <- vector("list", length = 5+length(results.list.names))
        names(Results_DML) <- c("DML_iteration", "DML_fold_seed", "DML_type", "K_folds", "method_name", results.list.names)
        cat('* Initial Results list : ', '\n')
        print(Results_DML)
        
        Results_ML_Performance <- list(var_label = c(),
                                DML_iteration = c(),
                                DML_fold_seed = c(),
                                target_name = c(), 
                                method_name = c(), 
                                train_MSE = c(), 
                                xval_MSE = c(),
                                parameters = c(),
                                seed = c())  
      }
      
      cat('* FILE : 50_00_DML_Procedure_for_GLM.R, Iteration = ', i , '\n')
      source(file = '50_00_DML_Procedure_for_GLM.R')
      
      cat('* FILE : 50_00_DML_Procedure_for_DRF.R, Iteration = ', i , '\n')
      source(file = '50_00_DML_Procedure_for_DRF.R')
      
      cat('* FILE : 50_00_DML_Procedure_for_GBM.R, Iteration = ', i , '\n')
      source(file = '50_00_DML_Procedure_for_GBM.R')
      
      cat('* FILE : 50_00_DML_Procedure_for_DL.R, Iteration = ', i , '\n')
      source(file = '50_00_DML_Procedure_for_DL.R')
      
      cat('* FILE : 50_00_DML_Procedure_for_Ensemble_Best.R, Iteration = ', i , '\n')
      source(file = '50_00_DML_Procedure_for_Ensemble_Best.R')
      
      cat('* Output ML performance and DML results, Iteration = ', i , '\n')
      {
        Results_ML_Performance <- Results_ML_Performance %>% as.data.frame() %>% setDT()
        Results_DML <-  Results_DML %>% as.data.frame() %>% setDT()
        cat('* ML Algoritm Performance data frame ... ', '\n')
        Results_ML_Performance %>% print()
        cat('* DML Results data frame ... ', '\n')
        Results_DML %>% print()
        
        cat('* Write ML performance and DML results to a CSV, Iteration = ', i , '\n')
        {
          if(i==1){
            fwrite(Results_ML_Performance , file =  paste(project.path, "Output/Final/", paste(filename.begin, "Results_ML_Performance", filename.end,sep = "_"), ".csv", sep = ""), append = FALSE)
            fwrite(Results_DML , file =  paste(project.path, "Output/Final/", paste(filename.begin, "Results_DML", filename.end, sep = "_"), ".csv", sep = ""), append = FALSE)
          }else{
            fwrite(Results_ML_Performance , file =  paste(project.path, "Output/Final/", paste(filename.begin, "Results_ML_Performance", filename.end, ".csv",sep = "_"), sep = ""), append = TRUE)
            fwrite(Results_DML , file =  paste(project.path, "Output/Final/", paste(filename.begin, "Results_DML", filename.end, sep = "_"), ".csv", sep = ""), append = TRUE)
          }  
        }
      }
    }
  
    cat('* Remove/Restore object created in Grid search and DML process, Iteration = ', i , '\n')
    {
      # rm(seed_h2o); 
      final_df_h2o <- final_df_h2o[, final_df_h2o.colnames]
      print(h2o.ls())
    }
  }
}

