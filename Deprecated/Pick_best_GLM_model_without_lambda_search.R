# ***********************************************
# sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
cat('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>', '\n')
cat('Begin Running file : ', "Pick_best_GLM_model_without_lambda_search.R", " ...", '\n')
# sink()
# ***********************************************

best_glm_grid_no_lambda_search_list <- list(outcome =c(),
                                            outcome_type = c(),
                                            alpha = c(), 
                                            lambda = c(), 
                                            train_evalmetric = c(), 
                                            valid_evalmetric = c())

for(sp in 1:times.split.grid){
  # Splitting data into training and validation frame
  datause.split <- h2o.splitFrame(data=final.df.h2o, ratios=0.8);
  datause.train <- datause.split[[1]]
  datause.valid <- datause.split[[2]]
  cat('Dimension of training frame : ', '\n')
  print(h2o.dim(datause.train))
  cat('Dimension of validation frame : ', '\n')
  print(h2o.dim(datause.valid))
  
  # do it for Y D and Z
  time.ML_algo = Sys.time()
  for(var.y in ML_outcome.list) {
    fam = "gaussian"
    evalmetric = "mse"
    dec = FALSE
    var.y.type = "cont"
    
    if(h2o.isfactor(datause.train[,var.y])){
      fam = "binomial"
      evalmetric = "auc" 
      dec = TRUE
      var.y.type = "bin"
      cat(var.y, ' is a factor variable.' , '\n')
    }
    
    cat('Run GLM grid without lambda search ... ' , '\n')
    time <- Sys.time()
    cat('Target variable is : ' ,'\n')
    print(var.y)
    cat('---------------------------', '\n')
    glm_grid_no_lambda_search <- h2o.grid(algorithm = "glm", 
                                          x = var.x, 
                                          y = var.y,
                                          grid_id = "glm_grid_no_lambda_search",
                                          training_frame = datause.train,
                                          validation_frame = datause.valid,
                                          solver = "L_BFGS",
                                          hyper_params = glm_params_no_lambda_search,
                                          nfolds = cv.num.folds, 
                                          standardize = TRUE, 
                                          lambda_search = FALSE,
                                          family = fam,
                                          interaction_pairs = list(c("dayOfWeek", "nbhd")), 
                                          interactions = c("year", "week", "nbhd")
    )
    glm_grid_no_lambda_search_perf <- h2o.getGrid(grid_id = "glm_grid_no_lambda_search", sort_by = evalmetric , decreasing = dec)
    print(glm_grid_no_lambda_search_perf)
    cat('Time taken by GLM (without lambda search) : ', '\n')
    print(Sys.time() - time)
    best_glm_grid_no_lambda_search <- h2o.getModel(glm_grid_no_lambda_search_perf@model_ids[[1]])
    
    best_glm_grid_no_lambda_search_list$outcome <- c(best_glm_grid_no_lambda_search_list$outcome, var.y)
    best_glm_grid_no_lambda_search_list$outcome_type <- c(best_glm_grid_no_lambda_search_list$outcome_type, var.y.type)
    best_glm_grid_no_lambda_search_list$alpha <- c(best_glm_grid_no_lambda_search_list$alpha, best_glm_grid_no_lambda_search@parameters$alpha)
    best_glm_grid_no_lambda_search_list$lambda <- c(best_glm_grid_no_lambda_search_list$lambda, best_glm_grid_no_lambda_search@parameters$lambda)
    best_glm_grid_no_lambda_search_list$train_evalmetric <- c(best_glm_grid_no_lambda_search_list$train_evalmetric, h2o.mse(object = best_glm_grid_no_lambda_search, train = TRUE, valid = FALSE))
    best_glm_grid_no_lambda_search_list$valid_evalmetric <- c(best_glm_grid_no_lambda_search_list$valid_evalmetric, h2o.mse(object = best_glm_grid_no_lambda_search, train = FALSE, valid = TRUE))
    
    h2o.rm(best_glm_grid_no_lambda_search)
    h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "glm_grid_no_lambda_search"))
    rm(glm_grid_no_lambda_search)
    rm(glm_grid_no_lambda_search_perf)
    gc()
    gc()
    gc()
    h2o.ls()
    h2o:::.h2o.garbageCollect()
    h2o:::.h2o.garbageCollect()
    h2o:::.h2o.garbageCollect()
    h2o.ls()
  }
  cat('time taken by GLM grid search (without lambda search): ' , '\n')
  print(Sys.time() - time.ML_algo)
}

best_glm_grid_no_lambda_search_df <- best_glm_grid_no_lambda_search_list %>%
  as.data.frame() %>%
  group_by(outcome, outcome_type, alpha, lambda) %>%
  summarise(n = n(), train_evalmetric = mean(train_evalmetric, na.rm = TRUE), valid_evalmetric = mean(valid_evalmetric, na.rm = TRUE)) %>% 
  group_by(outcome, outcome_type) %>% 
  arrange(desc(n)) %>% 
  filter(row_number()==1) 
# =====================================================================================================================
save(best_glm_grid_no_lambda_search_df, 
     file = paste(project.path,"Output/Final/", output.filename ,"GridSearchResults.RData", sep="")) # *** Remove this in the final program ***
rm(best_glm_grid_no_lambda_search_df)
gc()

# ***********************************************
# sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
cat(' ... ', '\n')
cat('End Running file : ', "Pick_best_GLM_model_without_lambda_search.R", '\n')
cat('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<', '\n')
# sink()  # *** Keep in the final run ***
# ***********************************************

