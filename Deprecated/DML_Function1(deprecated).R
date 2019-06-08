GLM <- function(datause, dataout, ){
  cat("Dimension of data in use : ", dim(datause))
  cat("Splitting data in use into training and validation data ...")
  datause.split <- h2o.splitFrame(data, ratios = 0.75)
  train <- datause.split[[1]]
  valid <- datause.split[[2]]
  cat("Dimension of data held out: ", dim(dataout))
  cat("Running ML Algorithm ...")
  glm1 <- h2o.glm(x=x, 
                  y=y, 
                  training_frame = train, 
                  nfolds = 5, 
                  validation_frame = valid, 
                  fold_assignment = "Random", 
                  family = "gaussian", 
                  alpha = 1, 
                  lambda = 0.5, 
                  lambda_search = TRUE,
                  early_stopping= TRUE, 
                  remove_collinear_columns = TRUE, 
                  interaction_pairs=interaction_list
  )
  
}
coef <- glm1@model$coefficients
coef.table <- glm1@model$coefficients_table
coef.table.standard <- glm1@model$standardized_coefficient_magnitudes

h2o.num_iterations(glm1)
h2o.null_dof(glm1, train = TRUE, valid = TRUE, xval = TRUE)
h2o.residual_dof(glm1, train = TRUE, valid = TRUE, xval = TRUE)
h2o.mse(glm1, train = TRUE, valid = TRUE, xval = TRUE)
h2o.r2(glm1, train = TRUE, valid = TRUE, xval = TRUE)
h2o.logloss(glm1, train = TRUE, valid = TRUE, xval = TRUE)
h2o.auc(glm1, train = TRUE, valid = TRUE, xval = TRUE)
h2o.giniCoef(glm1, train = TRUE, valid = TRUE, xval = TRUE)
h2o.null_deviance(glm1, train = TRUE, valid = TRUE, xval = TRUE)
h2o.residual_deviance(glm1, train = TRUE, valid = TRUE, xval = TRUE)
h2o.aic(glm1, train = TRUE, valid = TRUE, xval = TRUE)


glm1@model$scoring_history
print(h2o.performance(glm1, dataout))


# Elements to return
datause$yhatuse <- h2o.predict(object = glm1, newdata = datause)
datause$resuse <- datause$qdemand - datause$yhatuse
dataout$yhatout <- h2o.predict(object = glm1, newdata = dataout)
dataout$resout <- dataout$qdemand - dataout$yhatout