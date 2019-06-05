# 22_01_Reduce_dimensionality_of_reviews_data.R
source(file = '00_00_Preamble__current.R')

cat('* Load Library ... ', '\n')
{
  library(h2o)
  library(magrittr)
  library(plyr)
  library(plotly)
}

cat('* Take inputs ... ', '\n')
{
  n.threads = -1 
  port = 11114
  max.mem = '550G'
  max_text_features = 50000
  # For K
  gamma <- 10
  max.compress = 10 # maximum allowed compression (in percentage of total number of columns)
  min.compress = 50 # minimum allowed compression (in percentage of total number of columns)
  compress.intervals = 3 # Number of intervals between maximum and minimum compression
  # For regularization
  gamma_range <- c(0.1, 10, 1000)
  k.val <- 400
}

Data_Set_List <- c()

# This file is to construct rental features in low dimension using GLRM and obtain the archetypes
h2o.init(port = port, nthreads = n.threads, max_mem_size = max.mem)
h2o.removeAll()

cat('* Load and prepare data on H2O .... ', '\n')
{
  if(file.exists(paste(project.path, "Output/TEMP/ForH2O_Reviews_GLRM/", "22_01_Reviews_Panel", ".csv", sep = ""))){
    cat('* File : ' , paste(project.path, "Output/TEMP/ForH2O_Reviews_GLRM/", "22_01_Reviews_Panel", ".csv", sep = "") ,' EXISTS.', '\n')
    assign("Reviews_Panel" , h2o.importFile(path = paste(project.path, "Output/TEMP/ForH2O_Reviews_GLRM/", "22_01_Reviews_Panel", ".csv", sep = ""), 
                                          destination_frame = "Reviews_Panel", 
                                          header = TRUE))
    cat('* Dimension of this data : ', '\n')
    print(h2o.dim(get("Reviews_Panel")))
    cat('* Add to the list Data_Set_List ... ', '\n')
    Data_Set_List <- c(Data_Set_List, get("Reviews_Panel"))
    cat('* ^Importing done. ', '\n')
  }  
  
  h2o.ls() %>% print()
  for(i in seq(from = 1, to = max_text_features, by = 200)){
    if(file.exists(paste(project.path, "Output/TEMP/ForH2O_Reviews_GLRM/", "22_01_Reviews_tfidf_200_data_", as.character(i), ".csv", sep = ""))){
      cat('* File : ' , paste(project.path, "Output/TEMP/ForH2O_Reviews_GLRM/", "22_01_Reviews_tfidf_200_data_", as.character(i), ".csv", sep = "") ,' EXISTS.', '\n')
      assign(paste("Reviews_tfidf_200_data_", as.character(i), sep = ""), 
             h2o.importFile(path = paste(project.path, 
                                         "Output/TEMP/ForH2O_Reviews_GLRM/", 
                                         "22_01_Reviews_tfidf_200_data_", 
                                         as.character(i), 
                                         ".csv", 
                                         sep = ""), 
                            destination_frame = paste("Reviews_tfidf_200_data_", as.character(i), sep = ""), 
                            header = TRUE))
      cat('* Dimension of this data : ', '\n')
      print(h2o.dim(get(paste("Reviews_tfidf_200_data_", as.character(i), sep = ""))))
      cat('* Add to the list Data_Set_List ... ', '\n')
      Data_Set_List <- c(Data_Set_List, get(paste("Reviews_tfidf_200_data_", as.character(i), sep = "")))
      cat('* ^Importing done. ', '\n')
      print(h2o.ls())
    }
    else{
      cat('* File : ' , paste(project.path, "Output/TEMP/ForH2O_Reviews_GLRM/", "22_01_Reviews_tfidf_200_data_", as.character(i), ".csv", sep = "") ,' DOES NOT EXIST.', '\n')
      break
    }
  }
  
  print(h2o.ls())
  cat('* Length of the list containing all the data : ', length(Data_Set_List), '\n')
  cat('Binding data by columns' , '\n')
  H2O_data_for_GLRM <- do.call("h2o.cbind", args = Data_Set_List)
  cat('* Final data dimensions for GLRM: ', '\n')
  print(h2o.dim(H2O_data_for_GLRM))
}

cat('* Perform GLRM', '\n')
{
  h2o.colnames(H2O_data_for_GLRM)
  # Define which columns are catagorical columns and set them as factor or enum varable. 
  # In reviews tfidf data there is no catagorical column.
  cat_cols <- c()
  for(i in cat_cols){
    H2O_data_for_GLRM[[i]] <- as.factor(H2O_data_for_GLRM[[i]])
  }
  glrm_cols <- colnames(H2O_data_for_GLRM)[!(colnames(H2O_data_for_GLRM) %in% c("propertyid", "rev_date", "reviewer_id", "id"))]
  
  losses <- data.frame('index' = which(colnames(H2O_data_for_GLRM) %in% cat_cols) - 1,
                       'loss' = rep("Categorical", length(cat_cols)), stringsAsFactors = FALSE)
  
  # Split the data into training and validation frame
  random.seed <- sample(1:999999, 1, replace=TRUE)
  H2O_data_for_GLRM <- h2o.assign(H2O_data_for_GLRM, "H2O_data_for_GLRM.hex")
  miss_data <- h2o.assign(H2O_data_for_GLRM, "miss_data.hex")
  h2o.insertMissingValues(data = miss_data, fraction = 0.10, seed = random.seed)
  # Dimensions of training data
  print(dim(miss_data))
  # Dimensions of validation data
  print(dim(H2O_data_for_GLRM))
  

  cat('* Effect of the rank ... ', '\n')
  {
    k_range <- seq(ceiling(max.compress*length(glrm_cols)/100), 
                   ceiling(min.compress*length(glrm_cols)/100), 
                   by = ceiling(((min.compress*length(glrm_cols)/100)-(max.compress*length(glrm_cols)/100))/compress.intervals)
    )
    
    random.seed <- sample(1:999999, 1, replace=TRUE)

    k_models <- llply(k_range, function(k)
      h2o.glrm(training_frame = miss_data, cols = glrm_cols, validation_frame = H2O_data_for_GLRM,
               model_id = paste0("glrm_k_", k), seed = random.seed,
               k = k, gamma_x = gamma, gamma_y = gamma,
               regularization_x = "L1", regularization_y = "L1",   # "L1" "Quadratic"
               transform = "STANDARDIZE", impute_original = TRUE))
    
    # Plot Numeric Error and Rank
    
    k_error <- ldply(k_models, function(ml) 
      data.frame('k' = ml@parameters$k, 
                 'numeric_error' = c(h2o.performance(ml, valid = FALSE)@metrics$numerr,
                                     h2o.performance(ml, valid = TRUE)@metrics$numerr),
                 'type' = c("training", "validation"))
    )
    plot_ly(data = k_error, x = ~k, y = ~numeric_error, mode = "markers+lines", color = ~type, text = ~paste0("k: ", k))
  }
  
  cat('* Effect of the regularization ... ', '\n')
  {
    gamma_models <- llply(gamma_range, function(g) 
      h2o.glrm(training_frame = miss_data, cols = glrm_cols, validation_frame = H2O_data_for_GLRM, 
               model_id = paste0("glrm_gamma_", g), seed = random.seed,
               k = k.val, gamma_x = g, gamma_y = g, 
               regularization_x = "L1", regularization_y = "L1", 
               transform = "STANDARDIZE", impute_original = TRUE))
    
    # Plot Numeric Error and Regularization Strength/Gamma
    gamma_error <- ldply(gamma_models, function(ml) 
      data.frame('gamma' = ifelse(is.null(ml@parameters$gamma_x), 0,  ml@parameters$gamma_x),
                 'numeric_error' = c(h2o.performance(ml, valid = FALSE)@metrics$numerr,
                                     h2o.performance(ml, valid = TRUE)@metrics$numerr),
                 'type' = c("training", "validation"))
    )
    
    plot_ly(data = gamma_error, x = ~gamma, y = ~numeric_error, mode = "markers+lines", 
            color = ~type, text = ~paste0("gamma: ", gamma))
  }
  
  cat('* Final Model ... ', '\n')
  {
    
  }
}
