# ***********************************************
# sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
cat('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>', '\n')
cat('Begin Running file : ', "DML_Function.R", " ...", '\n')
# sink()
# ***********************************************

# Trigger an explicit garbage collection across all nodes in the H2O cluster.
.h2o.garbageCollect <- function() {
  res <- .h2o.__remoteSend("GarbageCollect", method = "POST")
}

# ------------
glm_nolambda_search <- function(var.x, var.y, datause,dataout,n.folds, alpha, lambda, family, lambda.search=FALSE){
  start_time <- NULL
  fit_glm <- NULL
  yhatout <- NULL
  resout <- NULL
  train.mse <- NULL
  cv.mse <- NULL
  valid.mse <- NULL
  end_time <- NULL
  start_time <- Sys.time()
  
  fit_glm <- h2o.glm(x = var.x,
                     y = var.y, 
                     model_id = "fit_glm",
                     training_frame = datause, 
                     validation_frame = dataout, 
                     nfolds = n.folds, 
                     alpha = alpha, 
                     lambda = lambda,
                     lambda_search = lambda.search,
                     family = family, 
                     solver = "L_BFGS" 
  )
  yhatout <- h2o.predict(fit_glm, dataout)
  resout <- as.matrix(as.data.frame(dataout[,var.y] - yhatout))
  if(family == "binomial") 
    { resout <- as.matrix(as.data.frame(dataout[,var.y] - yhatout[, "p1"])) } 
  else 
    { resout <- as.matrix(as.data.frame(dataout[,var.y] - yhatout)) }
  train.mse <- h2o.mse(object = fit_glm, train = TRUE, xval = FALSE, valid = FALSE)
  cv.mse <- h2o.mse(object = fit_glm, train = FALSE, xval = TRUE, valid = FALSE)
  valid.mse <- h2o.mse(object = fit_glm, train = FALSE, xval = FALSE, valid = TRUE)
  # test.mse <- h2o.mse(h2o.performance(model = fit_glm,newdata = dataout))
  # Removing all the objects produced by the grid and cross validations
  h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "fit_glm"))
  end_time <- Sys.time()
  return(list(resout = resout,
              train.mse = train.mse,
              cv.mse = cv.mse,
              test.mse = valid.mse,
              run.time = (end_time - start_time))
  )
}


glm_lambda_search <- function(var.x, var.y, datause,dataout,n.folds, alpha, family, lambda.search=TRUE){
  start_time <- NULL
  fit_glm <- NULL
  yhatout <- NULL
  resout <- NULL
  train.mse <- NULL
  cv.mse <- NULL
  valid.mse <- NULL
  end_time <- NULL
  start_time <- Sys.time()
  
  fit_glm <- h2o.glm(x = var.x,
                     y = var.y, 
                     model_id = "fit_glm",
                     training_frame = datause, 
                     validation_frame = dataout, 
                     nfolds = n.folds, 
                     alpha = alpha, 
                     lambda_search = lambda.search,
                     family = family, 
                     solver = "IRLSM" 
  )
  yhatout <- h2o.predict(fit_glm, dataout)
  resout <- as.matrix(as.data.frame(dataout[,var.y] - yhatout))
  if(family == "binomial") 
  { resout <- as.matrix(as.data.frame(dataout[,var.y] - yhatout[, "p1"])) } 
  else 
  { resout <- as.matrix(as.data.frame(dataout[,var.y] - yhatout)) }
  train.mse <- h2o.mse(object = fit_glm, train = TRUE, xval = FALSE, valid = FALSE)
  cv.mse <- h2o.mse(object = fit_glm, train = FALSE, xval = TRUE, valid = FALSE)
  valid.mse <- h2o.mse(object = fit_glm, train = FALSE, xval = FALSE, valid = TRUE)
  # test.mse <- h2o.mse(h2o.performance(model = fit_glm,newdata = dataout))
  # Removing all the objects produced by the grid and cross validations
  h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "fit_glm"))
  end_time <- Sys.time()
  return(list(resout = resout,
              train.mse = train.mse,
              cv.mse = cv.mse,
              test.mse = valid.mse,
              run.time = (end_time - start_time))
  )
}


# ------------
glm_ml1 <- function(datause, dataout, var.y, var.x, split.ratio, num.folds, glm_params) {
  start_time <- Sys.time()
  
  # Inputs
  datause.split <- NULL
  datause.train <- NULL
  datause.valid <- NULL
  glm_grid <- NULL
  glm_grid_perf <- NULL
  best_glm <- NULL
  yhatout <- NULL
  resout <- NULL
  train.mse <- NULL
  valid.mse <- NULL
  test.mse <- NULL
  
  
  # Splitting data into training and validation frame
  datause.split <- h2o.splitFrame(data=datause, ratios=split.ratio)
  datause.train <- datause.split[[1]]
  datause.valid <- datause.split[[2]]
  h2o.rm(datause.split)
  
  # Train and validate a cartesian grid of GLMs
  glm_grid <- h2o.grid(algorithm = "glm", 
                       x = var.x, 
                       y = var.y,
                       grid_id = "glm_grid",
                       training_frame = datause.train,
                       validation_frame = datause.valid,
                       hyper_params = glm_params,
                       nfolds = num.folds, 
                       standardize = TRUE, 
                       lambda_search = FALSE
                       )
  
  glm_grid_perf <- h2o.getGrid(grid_id = "glm_grid", sort_by = "mse", decreasing = FALSE)
  best_glm <- h2o.getModel(glm_grid_perf@model_ids[[1]])
  yhatout <- h2o.predict(best_glm, dataout)
  resout <- as.matrix(as.data.frame(dataout[,var.y] - yhatout))
  train.mse <- h2o.mse(object = best_glm, train = TRUE, valid = FALSE)
  valid.mse <- h2o.mse(object = best_glm, train = FALSE, valid = TRUE)
  test.mse <- h2o.mse(h2o.performance(model = best_glm,newdata = dataout))
  
  # Outputs
  cat("Grid performance (all hyper-paramenters): ", "\n")
  cat("---------------------------------------------------------------------------------------------------------------------------------------", "\n")
  print(glm_grid_perf)
  cat("=======================================================================================================================================", "\n")
  
  cat("Best Model Summary : ", "\n")
  cat("---------------------------------------------------------------------------------------------------------------------------------------", "\n")
  print(best_glm@model[["model_summary"]])
  cat("=======================================================================================================================================", "\n")
  
  cat("Best Model MSE : ", "\n")
  cat("---------------------------------------------------------------------------------------------------------------------------------------", "\n")
  # print(best_glm@model$training_metrics)
  # print(best_glm@model$validation_metrics)
  cat("On training data : ", train.mse, "\n")
  cat("On validation data : ", valid.mse, "\n")
  cat("On test data : ", test.mse, "\n")
  cat("=======================================================================================================================================", "\n")
  
  # Removing all the objects produced by the grid and cross validations
  h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "glm_grid"))
  
  end_time <- Sys.time()
  return(list(resout = resout, 
              train.mse = train.mse, 
              valid.mse = valid.mse, 
              test.mse = test.mse, 
              run.time = (end_time - start_time))
         )
  
}
# -----------
# ***********************************************
# sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
cat(' ... ', '\n')
cat('End Running file : ', "DML_Function.R", '\n')
cat('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<', '\n')
# sink()  # *** Keep in the final run ***
# ***********************************************
