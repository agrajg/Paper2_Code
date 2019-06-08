cv.num.folds = 5

cat('* DML using GLM', '\n')
{
  for(j in 1:crossfit.K){
    # j=1
    cat('* Dataout sample (test) sample count - ', j, ' .','\n')
    cat('* Assigning datause and dataout for DML... ','\n')
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
    
    cat('* Initilizing output DS : this is local list that will store gmm results for each cross fit predicted data ... ', '\n')
    {
      results.gmm <-vector("list", length = length(results.list.names))
      names(results.gmm) <- results.list.names
      results.gmm[results.list.names] <- 0  
      # Initilizing cross fit predicted outcome to pool for DML2 
      resYX.pool <- vector()
      resDX.pool <- vector()
      resZX.pool <- vector()
    }
    
    resYX <- vector()
    for(target in target_list[1:4]){
      # target = target_list[1]
      # resyx <- vector()
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
      
      seed = sample(1:999999, 1, replace=TRUE)
      GLM_fit <- h2o.glm(model_id = paste("GLM", target, "iequals",as.character(i), "jequals",as.character(j), sep = "_"), 
                    alpha = Best_Model_Within_Method$GLM_alpha[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "GLM")], 
                    lambda = Best_Model_Within_Method$GLM_lambda[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "GLM")], 
                    x = predictor_list, 
                    y = target,
                    training_frame = datause, 
                    nfolds = cv.num.folds, 
                    seed = seed,
                    keep_cross_validation_predictions = TRUE,
                    standardize = TRUE, 
                    lambda_search = FALSE,
                    family = fam,
                    interaction_pairs = list(c("dayOfWeek", "nbhd")), 
                    interactions = c("year", "week", "nbhd"))
      
      h2o.saveModel(object=GLM_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)

      DRF_fit <- h2o.randomForest(model_id = paste("DRF", target, "iequals",as.character(i), "jequals",as.character(j), sep = "_"), 
                                  ntrees = Best_Model_Within_Method$DRF_ntrees[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "DRF")], 
                                  max_depth = Best_Model_Within_Method$DRF_max_depth[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "DRF")], 
                                  min_rows = Best_Model_Within_Method$DRF_min_rows[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "DRF")], 
                                  nbins = Best_Model_Within_Method$DRF_nbins[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "DRF")], 
                                  x = predictor_list, 
                                  y = target, 
                                  training_frame = datause, 
                                  nfolds = cv.num.folds, 
                                  keep_cross_validation_predictions = TRUE,
                                  seed = seed,
                                  stopping_rounds = 5,
                                  stopping_metric = "AUTO", 
                                  stopping_tolerance = 0.001,
                                  min_split_improvement = 1e-05
                                  )
      h2o.saveModel(object=DRF_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)
      
      GBM_fit <- h2o.gbm(model_id = paste("GBM", target, "iequals",as.character(i), "jequals",as.character(j), sep = "_"), 
                         learn_rate = Best_Model_Within_Method$GBM_learn_rate[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "GBM")], 
                         ntrees = Best_Model_Within_Method$GBM_ntrees[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "GBM")], 
                         max_depth = Best_Model_Within_Method$GBM_max_depth[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "GBM")], 
                         min_rows = Best_Model_Within_Method$GBM_min_rows[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "GBM")], 
                         nbins = Best_Model_Within_Method$GBM_nbins[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "GBM")], 
                         x = predictor_list, 
                         y = target, 
                         training_frame = datause, 
                         nfolds = cv.num.folds, 
                         seed = seed,
                         keep_cross_validation_predictions = TRUE,
                         learn_rate_annealing = 1, 
                         stopping_rounds = 5, 
                         stopping_metric = "AUTO", 
                         stopping_tolerance = 0.001, 
                         min_split_improvement = 1e-05)
      h2o.saveModel(object=GBM_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)
      
      DL_fit <- h2o.deeplearning(model_id = paste("DL", target, "iequals",as.character(i), "jequals",as.character(j), sep = "_"), 
                                 activation = as.character(Best_Model_Within_Method$DL_activation[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "DL")]), 
                                 hidden = strtoi(strsplit(as.character(Best_Model_Within_Method$DL_hidden[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "DL")]), split = "_")[[1]], base = 10L), 
                                 l1 = Best_Model_Within_Method$DL_l1[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "DL")], 
                                 l2 = Best_Model_Within_Method$DL_l2[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "DL")], 
                                 x = predictor_list, 
                                 y = target, 
                                 training_frame = datause, 
                                 nfolds = cv.num.folds, 
                                 keep_cross_validation_predictions = TRUE,
                                 seed = seed,
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
                                 sparse = FALSE
                                 )
      h2o.saveModel(object=DL_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)
      
      Ensemble_fit <- h2o.stackedEnsemble(model_id = paste("Ensemble", target, "iequals",as.character(i), "jequals",as.character(j), sep = "_"), 
                                          base_models = list(GLM_fit, DRF_fit, GBM_fit, DL_fit), 
                                          x = predictor_list, 
                                          y = target, 
                                          training_frame = datause, 
                                          seed = seed
                                          )
      h2o.saveModel(object=Ensemble_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)
      
      if(as.character(Best_Model_All_Methods$method_name[which(Best_Model_All_Methods$target_name == target)]) == "GLM"){
        Best_fit <- GLM_fit
      }
      if(as.character(Best_Model_All_Methods$method_name[which(Best_Model_All_Methods$target_name == target)]) == "DRF"){
        Best_fit <- DRF_fit
      }
      if(as.character(Best_Model_All_Methods$method_name[which(Best_Model_All_Methods$target_name == target)]) == "GBM"){
        Best_fit <- GBM_fit
      }
      if(as.character(Best_Model_All_Methods$method_name[which(Best_Model_All_Methods$target_name == target)]) == "DL"){
        Best_fit <- DL_fit
      }
      h2o.saveModel(object=Best_fit, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)
      
      

      h2o.rm(GLM_fit); h2o.rm(DRF_fit); h2o.rm(GBM_fit); h2o.rm(DL_fit); h2o.rm(Ensemble_fit); h2o.rm(Best_fit); 
      h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("GLM", target, "iequals",as.character(i), "jequals",as.character(j), sep = "_")));
      h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("DRF", target, "iequals",as.character(i), "jequals",as.character(j), sep = "_")));
      h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("GBM", target, "iequals",as.character(i), "jequals",as.character(j), sep = "_")));
      h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("DL", target, "iequals",as.character(i), "jequals",as.character(j), sep = "_")));
      h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), paste("Ensemble", target, "iequals",as.character(i), "jequals",as.character(j), sep = "_")));
      
      
      h2o.ls()
      # resyx <- yx[["resout"]]
      # resYX <- cbind(resYX, resyx)
    }
    
    resDX <- vector()
    for(target in D){
      # resdx <- vector()
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
      yx <- h2o.glm(model_id = paste("GLM", target, "iequals",as.character(i), "jequals",as.character(j), sep = "_"), 
                    alpha = Best_Model_Within_Method$GLM_alpha[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "GLM")], 
                    lambda = Best_Model_Within_Method$GLM_lambda[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "GLM")], 
                    x = predictor_list, 
                    y = target,
                    training_frame = datause, 
                    nfolds = cv.num.folds, 
                    standardize = TRUE, 
                    lambda_search = FALSE,
                    family = fam,
                    interaction_pairs = list(c("dayOfWeek", "nbhd")), 
                    interactions = c("year", "week", "nbhd"))
      h2o.saveModel(object=yx, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)
      
      h2o.ls()
      h2o.rm(paste("GLM", target, "iequals",as.character(i), "jequals",as.character(j), sep = "_"))
      
      # resdx <- dx[["resout"]]
      # resDX <- cbind(resDX, resdx)
    }
    
    resZX <- vector()
    for(z in Z){
      # reszx <- vector()
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
      yx <- h2o.glm(model_id = paste("GLM", target, "iequals",as.character(i), "jequals",as.character(j), sep = "_"), 
                    alpha = Best_Model_Within_Method$GLM_alpha[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "GLM")], 
                    lambda = Best_Model_Within_Method$GLM_lambda[which(Best_Model_Within_Method$target_name == target & Best_Model_Within_Method$method_name == "GLM")], 
                    x = predictor_list, 
                    y = target,
                    training_frame = datause, 
                    nfolds = cv.num.folds, 
                    standardize = TRUE, 
                    lambda_search = FALSE,
                    family = fam,
                    interaction_pairs = list(c("dayOfWeek", "nbhd")), 
                    interactions = c("year", "week", "nbhd"))
      h2o.saveModel(object=yx, path=paste(project.path, "Output/TEMP/" , sep = "") , force=TRUE)
      
      h2o.ls()
      h2o.rm(paste("GLM", target, "iequals",as.character(i), "jequals",as.character(j), sep = "_"))
      # reszx <- zx[["resout"]]
      # resZX <- cbind(resZX, reszx)
    }
    
  }
  
}
