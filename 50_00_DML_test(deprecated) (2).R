# NOTES : 
cat('* Begin Running file : ', '50_00_DML_test.R ', " ...", '\n')
cat('* --------------------------------------------------', '\n')
cat('* Setting up data ...', '\n')
{
  cat('* R Setup', '\n')
  cat('* -------', '\n')
  {
    cat('* Clearing up workspace ...', '\n')
    rm(list = ls())
    source(file = "00_00_Preamble.R")
    
    cat('* Loading packages needed in this program ...', '\n')
    packages<-c("slam","foreach", "doParallel", "parallel", "h2o", "stringr", "stringi", "gmm", "tidyverse", "tidyr", "dplyr", "data.table", "ggplot2")
    # remove.packages(packages, lib = .libPaths())
    # install.packages(packages, dependencies = TRUE)
    check.packages(packages)
    
    file.time <- Sys.time()
    cat('* R Setup', '\n')
    cat('* =======', '\n')
  }
  cat('* Feeding Inputs ... ', '\n')
  {
    cat('* H2O port ... ', '\n')
    {  
      n.threads = 66
      port.num = 11111
      max.mem = '550G'
      force.DL = TRUE
    }
    
    {
      max_text_features = 10000
    }
  }
  
  cat('* Start h2o ... ')
  {
    h2o.init(port = port.num, nthreads = n.threads, max_mem_size = max.mem, forceDL = force.DL)
    h2o.removeAll() ## clean slate - just in case the cluster was already running
    h2o.ls()
  }
  
  
  cat('* Load data on H2O')
  {
    if(file.exists(paste(project.path, "Output/TEMP/ForH2O/", "21_01_Demand_panel_DT", ".csv", sep = ""))){
      cat('* File : ' , paste(project.path, "Output/TEMP/ForH2O/", "21_01_Demand_panel_DT", ".csv", sep = "") ,' EXISTS.', '\n')
      assign("Demand_panel_DT" , h2o.importFile(path = paste(project.path, "Output/TEMP/ForH2O/", "21_01_Demand_panel_DT", ".csv", sep = ""), 
                                                destination_frame = "demand.data.h2o", 
                                                header = TRUE))
      cat('* ^Importing done. ', '\n')
    }  
    
    h2o.ls()
    for(i in seq(from = 1, to = max_text_features, by = 200)){
      if(file.exists(paste(project.path, "Output/TEMP/ForH2O/", "21_01_text_panel_DT_", as.character(i), ".csv", sep = ""))){
        cat('* File : ' , paste(project.path, "Output/TEMP/ForH2O/", "21_01_text_panel_DT_", as.character(i), ".csv", sep = "") ,' EXISTS.', '\n')
        assign(paste("text_panel_DT_", as.character(i), sep = ""), 
               h2o.importFile(path = paste(project.path, 
                                           "Output/TEMP/ForH2O/", 
                                           "21_01_text_panel_DT_", 
                                           as.character(i), 
                                           ".csv", 
                                           sep = ""), 
                              destination_frame = paste("text_panel_DT_", as.character(i), sep = ""), 
                              header = TRUE))
        
        cat('* ^Importing done. ', '\n')
        h2o.ls()
      }
      else{
        cat('* File : ' , paste(project.path, "Output/TEMP/ForH2O/", "21_01_text_panel_DT_", as.character(i), ".csv", sep = "") ,' DOES NOT EXIST.', '\n')
        break
      }
    }
    cat('Binding data by columns' , '\n')
    final.df.h2o <- h2o.cbind(Demand_panel_DT, 
                            text_panel_DT_1 , 
                            text_panel_DT_201, 
                            text_panel_DT_401, 
                            text_panel_DT_601)
    cat('* Final data dimensions : ', '\n')
    print(h2o.dim(final.df.h2o))
  }
  
  cat('* Creating relevant variables for DML.', '\n')
  {
    cat('* Making data nice and ready ... ', '\n')
    cat('* Y ...','\n')
    final.df.h2o$qdemand <- as.numeric(final.df.h2o$qdemand)
    cat('* D ...','\n')
    final.df.h2o$lprice_per_person <- as.numeric(final.df.h2o$lprice_per_person)
    
    cat('* Z ...','\n')
    final.df.h2o$prod_week1 <- as.numeric(final.df.h2o$prod_week1)
    final.df.h2o$prod_week2 <- as.numeric(final.df.h2o$prod_week2)
    final.df.h2o$prod_week3 <- as.numeric(final.df.h2o$prod_week3)
    final.df.h2o$prod_week4 <- as.numeric(final.df.h2o$prod_week4)
    final.df.h2o$prod_week5 <- as.numeric(final.df.h2o$prod_week5)
    cat('* Making Dummy variables production dummy ...','\n')
    for(pvar in c("prod_week1", "prod_week2", "prod_week3", "prod_week4", "prod_week5")) {
      temp.list = h2o.table(final.df.h2o[, pvar])[, pvar] %>% as.vector()
      cat('* Making Dummy variables production dummy ...','\n')
      for(i in temp.list) {
        cat('* Creating variable : ', paste(pvar, as.character(i), sep = "_") ,'\n')
        final.df.h2o[ , paste(pvar, as.character(i), sep = "_")] = h2o.ifelse(final.df.h2o[, pvar] == i, 1, 0)
        cat('* Creating variable : ', paste(pvar, as.character(i), "cap", sep = "_") ,'\n')
        final.df.h2o[ , paste(pvar, as.character(i), "cap", sep = "_")] = h2o.ifelse(final.df.h2o[, pvar] == i, final.df.h2o[, "capacity"], 0)
        final.df.h2o[ , paste(pvar, as.character(i), sep = "_")] = h2o.asfactor(final.df.h2o[ , paste(pvar, as.character(i), sep = "_")])
      }
      cat('* Creating variable : ',paste(pvar, "contcap", sep = "_") ,'\n')
      final.df.h2o[, paste(pvar, "contcap", sep = "_")] = final.df.h2o[, pvar]*final.df.h2o[, "capacity"]
    }
    
    cat('* X ...','\n')
    final.df.h2o$qdemand_l1 <- as.numeric(final.df.h2o$qdemand_l1)
    final.df.h2o$qdemand_l2 <- as.numeric(final.df.h2o$qdemand_l2)
    final.df.h2o$qdemand_l3 <- as.numeric(final.df.h2o$qdemand_l3)
    
    final.df.h2o$listingtype <- as.factor(final.df.h2o$listingtype)   
    final.df.h2o$bedrooms <- as.factor(final.df.h2o$bedrooms) 
    final.df.h2o$bathrooms <- as.factor(final.df.h2o$bathrooms) 
    final.df.h2o$nbhd <- as.factor(final.df.h2o$nbhd)
    final.df.h2o$latitude <- as.numeric(final.df.h2o$latitude)
    final.df.h2o$longitude <-  as.numeric(final.df.h2o$longitude)
    final.df.h2o$p_age <- as.numeric(final.df.h2o$p_age)
    final.df.h2o$h_age <- as.numeric(final.df.h2o$h_age)
    final.df.h2o$p_dayshosting <- as.numeric(final.df.h2o$p_dayshosting)
    final.df.h2o$h_dayshosting <- as.numeric(final.df.h2o$h_dayshosting)
    final.df.h2o$p_daysbooked <- as.numeric(final.df.h2o$p_daysbooked)
    final.df.h2o$h_daysbooked <- as.numeric(final.df.h2o$h_daysbooked)
    final.df.h2o$p_guestcount <- as.numeric(final.df.h2o$p_guestcount)
    final.df.h2o$h_guestcount <- as.numeric(final.df.h2o$h_guestcount)
    
    cat('* Rental ID ...','\n')
    final.df.h2o$propertyid <- as.factor(final.df.h2o$propertyid)
    
    cat('* handling date variables ...','\n')
    final.df.h2o$week <- as.factor(h2o.week(final.df.h2o$date))
    print(h2o.table(final.df.h2o$week))
    final.df.h2o$year <- as.factor(h2o.year(final.df.h2o$date))
    print(h2o.table(final.df.h2o$year))
    final.df.h2o$dayOfWeek <- as.factor(h2o.dayOfWeek(final.df.h2o$date))
    print(h2o.table(final.df.h2o$dayOfWeek))
    final.df.h2o$date <- as.factor(as.numeric(final.df.h2o$date))
    # =====================================================================================================================
    
    cat('* Defining variables.', '\n')
    cat('* -------------------', '\n')
    {
      cat('* Y ...','\n')
      Y <- c("qdemand")
      
      cat('* D ...','\n')
      D <- c("lprice_per_person")
      
      cat('* Z ...','\n')
      Z_alt <- c("prod_week1", "prod_week2", "prod_week3", "prod_week4", "prod_week5", 
                 "prod_week1_contcap", "prod_week2_contcap", "prod_week3_contcap", "prod_week4_contcap", "prod_week5_contcap")
      
      Z <- setdiff(str_subset(final.df.h2o %>% h2o.colnames(), "prod_week") , c("proddum",Z_alt))
      
      cat('* X ...','\n')
      X1 <- c("qdemand_l1","qdemand_l2" ,"qdemand_l3", "listingtype", "bedrooms", "bathrooms", "nbhd", "latitude","longitude", "p_age", "h_age" ,"p_dayshosting" ,
              "h_dayshosting" , "p_daysbooked" ,"h_daysbooked", "p_guestcount", "h_guestcount","propertyid", "date", "year", "week", "dayOfWeek")
      X2 <- str_subset(final.df.h2o %>% h2o.colnames(), "_text")
      X <- c(X1,X2)
      
      cat('* Defining variables.', '\n')
      cat('* ===================', '\n')
    }
    
    cat('* Printing variables ... ', '\n')
    {
      cat('* ==============================', '\n')
      cat('* Y : ', '\n')
      cat('* ------------------------------', '\n')
      print(Y)
      cat('* D : ', '\n')
      cat('* ------------------------------', '\n')
      print(D)
      cat('* Z : ', '\n')
      cat('* ------------------------------', '\n')
      print(Z)
      cat('* Z_alt : ', '\n')
      cat('* ------------------------------', '\n')
      print(Z_alt)
      cat('* X : ', '\n')
      cat('* ------------------------------', '\n')
      print(X)
      cat('* ==============================', '\n')
    }    
    
    cat('* Remove Clutter ... ', '\n')
    final.df.h2o <- final.df.h2o[, c(Y,D,Z,Z_alt,X)]
    
    cat('* Final data check ', '\n')
    cat('-------------------' , '\n')
    {
      cat('* DIM of the final data : ' , '\n')
      print(h2o.dim(final.df.h2o))
      cat('* SAMPLE of the final data : ' , '\n')
      print(final.df.h2o[11864:11869,1:10])
      cat('* STR of the final data : ')
      h2o.str(final.df.h2o)
      h2o.assign(final.df.h2o, "final_df_h2o")
      cat('* H2O Objects : ', '\n')
      h2o.ls()
      cat('* Final data check ', '\n')
      cat('* ================' , '\n')
    }
  }
}

cat('* MAIN PROGRAM', '\n')
cat('* ------------', '\n')
{
  cat('* Take all parmeters as inputs.','\n')
  {
    cat('* For ML algorithms ...', '\n')
    method_list = c("GLM", "DRF", "GBM", "DL")
    cv.num.folds=0                                # *** Change this in the final program
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
    hparams_DL = list(hidden_layer = c(200, 200, 200), 
                      activation = c("Rectifier", "Maxout", "Tanh"), 
                      l1 = c(0, 0.00001, 0.0001, 0.001, 0.01), 
                      l2 = c(0, 0.00001, 0.0001, 0.001, 0.01))
    
    search_criteria <- list(strategy = "RandomDiscrete", max_models = 5, stopping_tolerance = 0.0001)
    
    cat('* For DML algorithm ...', '\n')            
    times.split.dml = 1                           # *** Change this in the final program
    crossfit.K = 2                                # *** Change this in the final program
    cat(' Input fraction of data to go into model selection', '\n')
    model.select.data.frac <- (1/3)
    model.selection.data.split.ratio <- (4/5)
    
    cat('* Define predictors for all outcomes ... ')
    {
      predictor_list = X
      cat('* Define target list ... ', '\n')
      target_list = c(Y, D, Z)       # Keep this in final program
    }
    
    cat('* Take all parmeters as inputs.','\n')
    cat('* ================================','\n')
  }
  
  
  
  cat('* Repeating Model selection, DML 1 and DML 2 ', times.split.dml , 'times.','\n')
  cat('* Initialize a list to store performance of best model ... ', '\n')
  model_perf = list(iteration = c(), 
                    target_name = c(), 
                    method_name = c(), 
                    GLM_alpha = c(), 
                    GLM_lambda = c(),
                    eval_metric_name = c(), 
                    train_evalmetric_value = c(), 
                    valid_evalmetric_value = c()
                    )
  cat('* Initializing results lists etc of DML.','\n') 
  cat('* -------------------------------','\n') 
  {
    stats.name <- c("beta", "var")
    results.list.names <- apply(expand.grid(stats.name, D), 1, paste, collapse=".")
    cat('* Initializing DML 1 list : results.DML1.','\n')
    results.DML1 <- vector("list", length = length(results.list.names))
    names(results.DML1) <- results.list.names
    cat('* Initializing DML 2 list : results.DML2.','\n')
    results.DML2 <- vector("list", length = length(results.list.names))
    names(results.DML2) <- results.list.names
  }
  cat('* ===============================','\n') 
  cat('* ----------------------------------------------------------------------', '\n')
  
  
  
  for(i in 1:times.split.dml) {
    cat('* Split the data into model selection data and DML data', '\n')
    {
      final.df.h2o.split <- h2o.splitFrame(data=final.df.h2o, ratios=model.select.data.frac)
      cat('* Define model selction data ... ', '\n')
      model.selection.data <- final.df.h2o.split[[1]]
      cat('* Define DML data ... ', '\n')
      DML.data <- final.df.h2o.split[[2]]
    }
    
    cat('* Use model selection data to find best model in each categry (GLM, GRF, GBM, DL) ... ','\n')
    {
      cat('* Split model selection data into training and validation frame ... ','\n')
      {
        model.selection.data.split <- h2o.splitFrame(model.selection.data, ratios = model.selection.data.split.ratio)
        cat('* Defining training data ...', '\n')
        model.selection.data.train <- model.selection.data.split[[1]]
        cat('* Defining validation data ...', '\n')
        model.selection.data.valid <- model.selection.data.split[[2]]
      }
      
      cat('* Performing model selection exercise ... ', '\n')
      cat('Begin a loop for each target in target_list ... ', '\n')
      for(target in target_list[1:6]){
        cat('Check and change parmeters if the target is binary', '\n')
        # if(h2o.dim(h2o.unique(final.df.h2o[,target]))[1] == 2){
        if(h2o.isfactor(final.df.h2o[,target])){
          fam = "binomial"
          metricfunc = "h2o.auc"
          evalmetric = "auc" 
          dec = TRUE
          cat('* ', target, ' is a BINARY variable.' , '\n')
        }
        else{
          fam = "gaussian"
          metricfunc = "h2o.mse"
          evalmetric = "mse"
          dec = FALSE
          cat('* ', target, ' is a CONTINIOUS variable.' , '\n')
        }
        cat('* Begin loop for each method ... ', '\n')
        for(method in method_list){
          if(method == "GLM"){
            cat('* Formulate GLM grid for target : ', target, '.','\n')
            cat('* Train and validate a cartesian grid of GLMs ...')
            grid_GLM <- h2o.grid(algorithm = "glm", 
                                 hyper_params = hparams_GLM, 
                                 search_criteria = search_criteria,
                                 grid_id = "grid_GLM" , 
                                 x = predictor_list, 
                                 y = target, 
                                 training_frame = model.selection.data.train, 
                                 validation_frame = model.selection.data.valid, 
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
            # print(best_GLM)
            
            cat('Writing the GLM performance list ... ', '\n')
            {
              model_perf$iteration <- c(model_perf$iteration, i)
              model_perf$target_name <- c(model_perf$target_name, target)
              model_perf$method_name <- c(model_perf$method_name, method)
              model_perf$GLM_alpha <- c(model_perf$GLM_alpha , best_GLM@parameters$alpha)
              model_perf$GLM_lambda <- c(model_perf$GLM_lambda , best_GLM@parameters$lambda)
              model_perf$eval_metric_name <- c(model_perf$eval_metric_name, evalmetric)
              model_perf$train_evalmetric_value <- c(model_perf$train_evalmetric_value,do.call(metricfunc, args = list(object = best_GLM,train = TRUE,valid = FALSE)))
              model_perf$valid_evalmetric_value <- c(model_perf$valid_evalmetric_value,do.call(metricfunc,args = list(object = best_GLM,train = FALSE,valid = TRUE)))
            }
            print(model_perf)
            
            cat('* Remove the GLM grid ', '\n')
            {
              h2o.rm(best_GLM); h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "grid_GLM"))
              rm(grid_GLM); rm(grid_GLM_perf)
              gc(); gc(); gc(); h2o.ls()
              h2o:::.h2o.garbageCollect(); h2o:::.h2o.garbageCollect(); h2o:::.h2o.garbageCollect(); h2o.ls()
            }
          }
          
          if(method == "DRF"){
            cat('* Formulate DRF grid for target : ', target, '.','\n')
            cat('* Train and validate a cartesian grid of DRFs ...')
            # specify for classification problem 
            grid_DRF <- h2o.grid(algorithm = "randomForest", 
                                 hyper_params = hparams_DRF, 
                                 search_criteria = search_criteria,
                                 grid_id = "grid_DRF" , 
                                 x = predictor_list, 
                                 y = target, 
                                 training_frame = model.selection.data.train, 
                                 validation_frame = model.selection.data.valid, 
                                 nfolds = cv.num.folds, 
                                 min_split_improvement = 1e-05, 
                                 stopping_rounds = 0, 
                                 stopping_metric = c("AUTO"), 
                                 stopping_tolerance = 0.001,
                                 max_runtime_secs = 0, 
                                 seed = -1
            )
            cat('* Evaluating DRF grid performance ... ')
            grid_DRF_perf <- h2o.getGrid(grid_id = "grid_DRF", sort_by = evalmetric , decreasing = dec)
            cat('* Grid DRF models : ')
            print(grid_DRF_perf)
            cat('* Picking up the best DRF model ... ',  '\n')
            best_DRF <- h2o.getModel(grid_DRF_perf@model_ids[[1]])
            # print(best_DRF)
            
            cat('Writing the DRF performance list ... ', '\n')
            {
              model_perf$iteration <- c(model_perf$iteration, i)
              model_perf$target_name <- c(model_perf$target_name, target)
              model_perf$method_name <- c(model_perf$method_name, method)
              model_perf$GLM_alpha <- c(model_perf$GLM_alpha , NA)
              model_perf$GLM_lambda <- c(model_perf$GLM_lambda , NA)
              model_perf$eval_metric_name <- c(model_perf$eval_metric_name, evalmetric)
              model_perf$train_evalmetric_value <- c(model_perf$train_evalmetric_value,do.call(metricfunc, args = list(object = best_DRF,train = TRUE,valid = FALSE)))
              model_perf$valid_evalmetric_value <- c(model_perf$valid_evalmetric_value,do.call(metricfunc, args = list(object = best_DRF,train = FALSE,valid = TRUE)))
            }
            print(model_perf)
            cat('* Remove the DRF grid ', '\n')
            {
              h2o.rm(best_DRF); h2o.rm(ids = str_subset(as.vector(h2o.ls()[["key"]]), "grid_DRF"))
              rm(grid_DRF); rm(grid_DRF_perf)
              gc(); gc(); gc(); h2o.ls()
              h2o:::.h2o.garbageCollect(); h2o:::.h2o.garbageCollect(); h2o:::.h2o.garbageCollect(); h2o.ls()
            }
          }
          if(method == "GBM"){
            cat('Writing the performance list ... ', '\n')
            {
              model_perf$iteration <- c(model_perf$iteration, i)
              model_perf$target_name <- c(model_perf$target_name, target)
              model_perf$method_name <- c(model_perf$method_name, method)
              model_perf$GLM_alpha <- c(model_perf$GLM_alpha , NA)
              model_perf$GLM_lambda <- c(model_perf$GLM_lambda , NA)
              model_perf$eval_metric_name <- c(model_perf$eval_metric_name, evalmetric)
              # model_perf$train_evalmetric_value <- c(model_perf$train_evalmetric_value,do.call(metricfunc, args = list(object = best_GBM,train = TRUE,valid = FALSE)))
              model_perf$train_evalmetric_value <- c(model_perf$train_evalmetric_value,NA)
              # model_perf$valid_evalmetric_value <- c(model_perf$valid_evalmetric_value,do.call(metricfunc,args = list(object = best_GBM,train = FALSE,valid = TRUE)))
              model_perf$valid_evalmetric_value <- c(model_perf$valid_evalmetric_value,NA)
            }
            print(model_perf)
            
          }
          if(method == "DL"){
            cat('Writing the performance list ... ', '\n')
            {
              model_perf$iteration <- c(model_perf$iteration, i)
              model_perf$target_name <- c(model_perf$target_name, target)
              model_perf$method_name <- c(model_perf$method_name, method)
              model_perf$GLM_alpha <- c(model_perf$GLM_alpha , NA)
              model_perf$GLM_lambda <- c(model_perf$GLM_lambda , NA)
              model_perf$eval_metric_name <- c(model_perf$eval_metric_name, evalmetric)
              # model_perf$train_evalmetric_value <- c(model_perf$train_evalmetric_value,do.call(metricfunc, args = list(object = best_DL,train = TRUE,valid = FALSE)))
              model_perf$train_evalmetric_value <- c(model_perf$train_evalmetric_value,NA)
              # model_perf$valid_evalmetric_value <- c(model_perf$valid_evalmetric_value,do.call(metricfunc,args = list(object = best_DL,train = FALSE,valid = TRUE)))
              model_perf$valid_evalmetric_value <- c(model_perf$valid_evalmetric_value,NA)
            }
            print(model_perf)
          }
          
        } 
      }
    }
  }

  cat('Summary of models and their performance ... ', '\n')
  print(model_perf)
  print(model_perf %>% as.data.frame())
}


grid_GRF <- h2o.grid(algorithm = "glm", 
                     hyper_params = params_GRF, 
                     grid_id = "grid_GRF" , 
                     x = predictor_list, 
                     y = target, 
                     training_frame = model.selection.data.train, 
                     validation_frame = model.selection.data.valid,  
                     nfolds = cv.num.folds)
  # Split the data
  split     <- h2o.runif(final.df.h2o)
  cvgroup   <- as.numeric(cut(split,quantile(split,probs = seq(0, 1, 1/crossfit.K)),include.lowest = TRUE)) + 1
  # Initilizing output DS : this is local list that will store gmm results for each cross fit predicted data.
  # Store DML 1
  results.gmm.dml1 <-vector("list", length = length(results.list.names))
  names(results.gmm.dml1) <- results.list.names
  results.gmm.dml1[results.list.names] <- 0  
  # Store DML 2
  results.gmm.dml2 <-vector("list", length = length(results.list.names))
  names(results.gmm.dml2) <- results.list.names
  
  # Initilizing cross fit predicted outcome to pool for DML2 
  resYX.pool <- vector()
  resDX.pool <- vector()
  resZX.pool <- vector()
  # for each split of the date I perform a cross fit
  for(j in 1:crossfit.K){
    ii  <- cvgroup == j
    nii <- cvgroup != j
    if(crossfit.K==1){
      ii  <- cvgroup == j
      nii <- cvgroup == j
    }
    datause <- final.df.h2o[nii,]
    dataout <- final.df.h2o[ii,]
    # Estimation of nuisance parameters
    resYX <- vector()
    for(y in Y){
      cat('* target variable is : ' , '\n')
      print(y)
      fam = "gaussian"
      if(h2o.dim(h2o.unique(final.df.h2o[,y]))[1] == 2){ fam = "binomial" }
      resyx <- vector()
      yx <- glm_nolambda_search(predictor_list = predictor_list, 
                                target = y, 
                                datause = datause, 
                                dataout = dataout, 
                                n.folds = cv.num.folds, 
                                alpha = alpha, 
                                lambda = lambda,
                                family = fam, 
                                lambda.search = TRUE)
      resyx <- yx[["resout"]]
      resYX <- cbind(resYX, resyx)
    }
    
    resDX <- vector()
    for(d in D){
      cat('* target variable is : ' , '\n')
      print(d)
      fam = "gaussian"
      if(h2o.dim(h2o.unique(final.df.h2o[,d]))[1] == 2){ fam = "binomial" }
      resdx <- vector()
      dx <- glm_nolambda_search(predictor_list = predictor_list, 
                                target = d, 
                                datause = datause, 
                                dataout = dataout, 
                                n.folds = cv.num.folds, 
                                alpha = alpha,
                                lambda = lambda,
                                family = fam, 
                                lambda.search = TRUE)
      resdx <- dx[["resout"]]
      resDX <- cbind(resDX, resdx)
    }
    
    resZX <- vector()
    for(z in Z){          # *** Keep this in the final program ***
      # for(z in Z[1:4]){       # *** Remove this in the final program ***
      cat('* target variable is : ' , '\n')
      print(z)
      fam = "gaussian"
      if(h2o.dim(h2o.unique(final.df.h2o[,z]))[1] == 2){ fam = "binomial" }
      reszx <- vector()
      zx <- glm_nolambda_search(predictor_list = predictor_list, 
                                target = z, 
                                datause = datause, 
                                dataout = dataout, 
                                n.folds = cv.num.folds, 
                                alpha = alpha, 
                                lambda = lambda,
                                family = fam,
                                lambda.search = TRUE)
      reszx <- zx[["resout"]]
      resZX <- cbind(resZX, reszx)
    }
    
    gmm <- NULL
    gmm <- tsls(resYX~resDX,~resZX)
    for(lc in 1:length(D)){
      results.gmm.dml1[[(2*lc-1)]] <- results.gmm.dml1[[(2*lc-1)]] + (gmm$coefficients[[lc+1]]/crossfit.K) 
      results.gmm.dml1[[2*lc]] <- results.gmm.dml1[[2*lc]] + (gmm$vcov[lc+1,lc+1]/(crossfit.K^2))
    }
    resYX.pool <- rbind(resYX.pool, resYX)
    resDX.pool <- rbind(resDX.pool, resDX)
    resZX.pool <- rbind(resZX.pool, resZX)
  }
  gmm.pool <- NULL
  gmm.pool <- tsls(resYX.pool~resDX.pool,~resZX.pool)
  for(lc in 1:length(D)){
    results.gmm.dml2[[(2*lc-1)]] <-  (gmm$coefficients[[lc+1]]) 
    results.gmm.dml2[[2*lc]] <-  (gmm$vcov[lc+1,lc+1])
  }
  
  # Final results 
  results.DML1 <- rbind(as.data.frame(results.DML1), as.data.frame(results.gmm.dml1))
  results.DML2 <- rbind(as.data.frame(results.DML2), as.data.frame(results.gmm.dml2))
  
} #splits 
cat('* ======================================================================', '\n')
cat('* DML 1 results')
print(results.DML1)
cat('* DML 2 results')
print(results.DML2)

save(results.DML1, 
     file = paste(project.path,"Output/Final/", output.filename ,"DML1_Results.RData", sep="")) # *** Remove this in the final program ***

save(results.DML2, 
     file = paste(project.path,"Output/Final/", output.filename ,"DML2_Results.RData", sep="")) # *** Remove this in the final program ***



# ***********************************************
# # sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
cat('*  ... ', '\n')
cat('* End Running file : ', "Apply_DML_with_best_GLM_without_lambda_search.R", '\n')
cat('* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<', '\n')
# # sink()  # *** Keep in the final run ***
# ***********************************************
# =====================================================================================================================

# sink()                                                                                        # *** Keep in the final run ***
cat('* MAIN PROGRAM', '\n')
cat('* ============', '\n')




{

  # cat('* Subset Data (if needed)', '\n')
  # cat('* -----------------------', '\n')
  # {
  #   X <- X[1:30]                     # *** Remove this in the final program***
  #   Z <- Z[1:4]                      # *** Remove this in the final program***
  #   Z <- Z_alt                          # Alternate specification
  # 
  #   cat('* Subset Data (if needed).', '\n')
  #   cat('* ========================', '\n')
  # }
  
  cat('* Print all the inputs for DML Main program. ','\n')
  cat('* -------------------------------------------','\n')
  {
    cat('* General : ', '\n')
    cat('* ----------', '\n')
    cat('* file.num = ', file.num ,'\n') 
    cat('* method = ', method ,'\n')
    cat('* is.test = ', is.test,'\n')
    cat('* ==========', '\n')
    
    cat('* For ML algorithms : ', '\n')
    cat('* --------------------', '\n')
    cat('* cv.num.folds = ', cv.num.folds , '\n')
    cat('* grid = ', grid , '\n')
    cat('* gridGLM = ','\n')
    print(grid_GLM)
    cat('* ====================', '\n')
    
    cat('* For DML algorithm : ', '\n') 
    cat('* --------------------', '\n')
    cat('* times.split.dml = ', times.split.dml ,'\n')
    cat('* crossfit.K = ', crossfit.K ,'\n')
    cat('* ====================', '\n')
    
    cat('* Final Set of Variables for Model Selection and DML : ', '\n')
    cat('* -----------------------------------------------------', '\n')
    cat('* Y : ', '\n')
    cat('* -----------------------------------------------------', '\n')
    print(Y)
    cat('* =====================================================', '\n')
    
    cat('* D : ', '\n')
    cat('* -----------------------------------------------------', '\n')
    print(D)
    cat('* =====================================================', '\n')
    
    cat('* Z : ', '\n')
    cat('* -----------------------------------------------------', '\n')
    print(Z)
    cat('* =====================================================', '\n')
    
    cat('* X : ', '\n')
    cat('* -----------------------------------------------------', '\n')
    print(X)
    cat('* =====================================================', '\n')
    
    cat('* Print all the inputs for DML Main program. ','\n')
    cat('* ===========================================','\n')
  }

  cat('* Apply DML with best GLM without lambda search.','\n')
  cat('* ----------------------------------------------','\n')
  {
    cat('* Defining Predictor and Target Variables','\n') 
    cat('* -------------------------------','\n') 
    predictor_list = X                         
    tar  <- c(Y,D,Z)
    
  }
  
}



  cat('* Initializing results lists etc.','\n') 
  cat('* -------------------------------','\n') 
  {
    stats.name <- c("beta", "var")
    results.list.names <- apply(expand.grid(stats.name, D), 1, paste, collapse=".")
    cat('* Initializing DML 1 list : results.DML1.','\n')
    results.DML1 <- vector("list", length = length(results.list.names))
    names(results.DML1) <- results.list.names
    cat('* Initializing DML 2 list : results.DML2.','\n')
    results.DML2 <- vector("list", length = length(results.list.names))
    names(results.DML2) <- results.list.names
  }
  cat('* ===============================','\n') 
  
  cat('* Repeating Model selection, DML 1 and DML 2 ', times.split.dml , 'times.','\n')
  cat('* ----------------------------------------------------------------------', '\n')
  for(i in 1:times.split.dml) {
    i=1
    cat('* Split the data into model selection data and DML data', '\n')
    
    # Split the data
    split     <- h2o.runif(final.df.h2o)
    cvgroup   <- as.numeric(cut(split,quantile(split,probs = seq(0, 1, 1/crossfit.K)),include.lowest = TRUE)) + 1
    # Initilizing output DS : this is local list that will store gmm results for each cross fit predicted data.
    # Store DML 1
    results.gmm.dml1 <-vector("list", length = length(results.list.names))
    names(results.gmm.dml1) <- results.list.names
    results.gmm.dml1[results.list.names] <- 0  
    # Store DML 2
    results.gmm.dml2 <-vector("list", length = length(results.list.names))
    names(results.gmm.dml2) <- results.list.names
    
    # Initilizing cross fit predicted outcome to pool for DML2 
    resYX.pool <- vector()
    resDX.pool <- vector()
    resZX.pool <- vector()
    # for each split of the date I perform a cross fit
    for(j in 1:crossfit.K){
      ii  <- cvgroup == j
      nii <- cvgroup != j
      if(crossfit.K==1){
        ii  <- cvgroup == j
        nii <- cvgroup == j
      }
      datause <- final.df.h2o[nii,]
      dataout <- final.df.h2o[ii,]
      # Estimation of nuisance parameters
      resYX <- vector()
      for(y in Y){
        cat('* target variable is : ' , '\n')
        print(y)
        fam = "gaussian"
        if(h2o.dim(h2o.unique(final.df.h2o[,y]))[1] == 2){ fam = "binomial" }
        resyx <- vector()
        yx <- glm_nolambda_search(predictor_list = predictor_list, 
                                  target = y, 
                                  datause = datause, 
                                  dataout = dataout, 
                                  n.folds = cv.num.folds, 
                                  alpha = alpha, 
                                  lambda = lambda,
                                  family = fam, 
                                  lambda.search = TRUE)
        resyx <- yx[["resout"]]
        resYX <- cbind(resYX, resyx)
      }
      
      resDX <- vector()
      for(d in D){
        cat('* target variable is : ' , '\n')
        print(d)
        fam = "gaussian"
        if(h2o.dim(h2o.unique(final.df.h2o[,d]))[1] == 2){ fam = "binomial" }
        resdx <- vector()
        dx <- glm_nolambda_search(predictor_list = predictor_list, 
                                  target = d, 
                                  datause = datause, 
                                  dataout = dataout, 
                                  n.folds = cv.num.folds, 
                                  alpha = alpha,
                                  lambda = lambda,
                                  family = fam, 
                                  lambda.search = TRUE)
        resdx <- dx[["resout"]]
        resDX <- cbind(resDX, resdx)
      }
      
      resZX <- vector()
      for(z in Z){          # *** Keep this in the final program ***
        # for(z in Z[1:4]){       # *** Remove this in the final program ***
        cat('* target variable is : ' , '\n')
        print(z)
        fam = "gaussian"
        if(h2o.dim(h2o.unique(final.df.h2o[,z]))[1] == 2){ fam = "binomial" }
        reszx <- vector()
        zx <- glm_nolambda_search(predictor_list = predictor_list, 
                                  target = z, 
                                  datause = datause, 
                                  dataout = dataout, 
                                  n.folds = cv.num.folds, 
                                  alpha = alpha, 
                                  lambda = lambda,
                                  family = fam,
                                  lambda.search = TRUE)
        reszx <- zx[["resout"]]
        resZX <- cbind(resZX, reszx)
      }
      
      gmm <- NULL
      gmm <- tsls(resYX~resDX,~resZX)
      for(lc in 1:length(D)){
        results.gmm.dml1[[(2*lc-1)]] <- results.gmm.dml1[[(2*lc-1)]] + (gmm$coefficients[[lc+1]]/crossfit.K) 
        results.gmm.dml1[[2*lc]] <- results.gmm.dml1[[2*lc]] + (gmm$vcov[lc+1,lc+1]/(crossfit.K^2))
      }
      resYX.pool <- rbind(resYX.pool, resYX)
      resDX.pool <- rbind(resDX.pool, resDX)
      resZX.pool <- rbind(resZX.pool, resZX)
    }
    gmm.pool <- NULL
    gmm.pool <- tsls(resYX.pool~resDX.pool,~resZX.pool)
    for(lc in 1:length(D)){
      results.gmm.dml2[[(2*lc-1)]] <-  (gmm$coefficients[[lc+1]]) 
      results.gmm.dml2[[2*lc]] <-  (gmm$vcov[lc+1,lc+1])
    }
    
    # Final results 
    results.DML1 <- rbind(as.data.frame(results.DML1), as.data.frame(results.gmm.dml1))
    results.DML2 <- rbind(as.data.frame(results.DML2), as.data.frame(results.gmm.dml2))
    
  } #splits 
  cat('* ======================================================================', '\n')
  cat('* DML 1 results')
  print(results.DML1)
  cat('* DML 2 results')
  print(results.DML2)
  
  save(results.DML1, 
       file = paste(project.path,"Output/Final/", output.filename ,"DML1_Results.RData", sep="")) # *** Remove this in the final program ***
  
  save(results.DML2, 
       file = paste(project.path,"Output/Final/", output.filename ,"DML2_Results.RData", sep="")) # *** Remove this in the final program ***
  
  
  # ***********************************************
  # # sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
  cat('*  ... ', '\n')
  cat('* End Running file : ', "Apply_DML_with_best_GLM_without_lambda_search.R", '\n')
  cat('* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<', '\n')
  # # sink()  # *** Keep in the final run ***
  # ***********************************************
  # =====================================================================================================================
  
  # sink()                                                                                        # *** Keep in the final run ***
  cat('* MAIN PROGRAM', '\n')
  cat('* ============', '\n')
}
