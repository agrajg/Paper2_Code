rm(list = ls())
source(file = '00_00_Preamble__current.R')
library(h2o)
library(magrittr)

cat('* Take inputs ... ', '\n')
{
  n.threads = -1 
  port = 11112
  max.mem = '550G'
  
  max_text_features = 50000
}

Data_Set_List <- c()

# This file is to construct rental features in low dimension using GLRM and obtain the archetypes
h2o.init(port = port, nthreads = n.threads, max_mem_size = max.mem)
h2o.removeAll()

cat('* Load and prepare data on H2O .... ', '\n')
{
  if(file.exists(paste(project.path, "Output/TEMP/ForH2O_GLRM/", "21_01_all_char_DT", ".csv", sep = ""))){
    cat('* File : ' , paste(project.path, "Output/TEMP/ForH2O_GLRM/", "21_01_all_char_DT", ".csv", sep = "") ,' EXISTS.', '\n')
    assign("all_char_DT" , h2o.importFile(path = paste(project.path, "Output/TEMP/ForH2O_GLRM/", "21_01_all_char_DT", ".csv", sep = ""), 
                                              destination_frame = "all_char_DT", 
                                              header = TRUE))
    cat('* Dimension of this data : ', '\n')
    print(h2o.dim(get("all_char_DT")))
    cat('* Add to the list Data_Set_List ... ', '\n')
    Data_Set_List <- c(Data_Set_List, get("all_char_DT"))
    cat('* ^Importing done. ', '\n')
  }  
  
  h2o.ls() %>% print()
  for(i in seq(from = 1, to = max_text_features, by = 200)){
    if(file.exists(paste(project.path, "Output/TEMP/ForH2O_GLRM/", "21_01_bed_am_DT_", as.character(i), ".csv", sep = ""))){
      cat('* File : ' , paste(project.path, "Output/TEMP/ForH2O_GLRM/", "21_01_bed_am_DT_", as.character(i), ".csv", sep = "") ,' EXISTS.', '\n')
      assign(paste("bed_am_DT_", as.character(i), sep = ""), 
             h2o.importFile(path = paste(project.path, 
                                         "Output/TEMP/ForH2O_GLRM/", 
                                         "21_01_bed_am_DT_", 
                                         as.character(i), 
                                         ".csv", 
                                         sep = ""), 
                            destination_frame = paste("bed_am_DT_", as.character(i), sep = ""), 
                            header = TRUE))
      cat('* Dimension of this data : ', '\n')
      print(h2o.dim(get(paste("bed_am_DT_", as.character(i), sep = ""))))
      cat('* Add to the list Data_Set_List ... ', '\n')
      Data_Set_List <- c(Data_Set_List, get(paste("bed_am_DT_", as.character(i), sep = "")))
      cat('* ^Importing done. ', '\n')
      print(h2o.ls())
    }
    else{
      cat('* File : ' , paste(project.path, "Output/TEMP/ForH2O_GLRM/", "21_01_bed_am_DT_", as.character(i), ".csv", sep = "") ,' DOES NOT EXIST.', '\n')
      break
    }
  }
  
  h2o.ls()
  for(i in seq(from = 1, to = max_text_features, by = 200)){
    if(file.exists(paste(project.path, "Output/TEMP/ForH2O_GLRM/", "21_01_desc_DT_", as.character(i), ".csv", sep = ""))){
      cat('* File : ' , paste(project.path, "Output/TEMP/ForH2O_GLRM/", "21_01_desc_DT_", as.character(i), ".csv", sep = "") ,' EXISTS.', '\n')
      assign(paste("desc_DT_", as.character(i), sep = ""), 
             h2o.importFile(path = paste(project.path, 
                                         "Output/TEMP/ForH2O_GLRM/", 
                                         "21_01_desc_DT_", 
                                         as.character(i), 
                                         ".csv", 
                                         sep = ""), 
                            destination_frame = paste("desc_DT_", as.character(i), sep = ""), 
                            header = TRUE))
      cat('* Dimension of this data : ', '\n')
      print(h2o.dim(get(paste("desc_DT_", as.character(i), sep = ""))))
      cat('* Add to the list Data_Set_List ... ', '\n')
      Data_Set_List <- c(Data_Set_List, get(paste("desc_DT_", as.character(i), sep = "")))
      cat('* ^Importing done. ', '\n')
      print(h2o.ls())
    }
    else{
      cat('* File : ' , paste(project.path, "Output/TEMP/ForH2O_GLRM/", "21_01_desc_DT_", as.character(i), ".csv", sep = "") ,' DOES NOT EXIST.', '\n')
      break
    }
  }
  cat('* Length of the list containing all the data : ', length(Data_Set_List), '\n')
  cat('Binding data by columns' , '\n')
  H2O_data_for_GLRM <- do.call("h2o.cbind", args = Data_Set_List)
  cat('* Final data dimensions : ', '\n')
  print(h2o.dim(H2O_data_for_GLRM))
}

cat('* Perform GLRM', '\n')
{
  h2o.colnames(H2O_data_for_GLRM)
  cat_cols <- c('host_response_time','thumbnail_url_dum',	'medium_url_dum',	'picture_url_dum',	'xl_picture_url_dum',	'host_url_dum',
                'host_thumbnail_url_dum',	'host_picture_url_dum',	'host_verifications_email',	'host_verifications_phone',
                'host_verifications_facebook',	'host_verifications_linkedin',	'host_verifications_reviews',	'host_verifications_gid',
                'host_verifications_others',	'host_is_superhost_dum', 'host_has_profile_pic_dum',	'host_identity_verified_dum',
                'requires_license_dum',	'rgpv_dum',	'rgpp_dum','propertytype'	,'listingtype',	'cancellationpolicy',	'checkintime',
                'checkouttime',	'instantbookenabled')
  for(i in cat_cols){
    H2O_data_for_GLRM[[i]] <- as.factor(H2O_data_for_GLRM[[i]])
  }
  glrm_cols <- colnames(H2O_data_for_GLRM)[!(colnames(H2O_data_for_GLRM) %in% c("pid"))]

  
  losses <- data.frame('index' = which(colnames(H2O_data_for_GLRM) %in% cat_cols) - 1,
                       'loss' = rep("Categorical", 3), stringsAsFactors = FALSE)
  
  glrm_k <- 2
  gamma <- 2
  
  base_model <- h2o.glrm(training_frame = miss_data, cols = glrm_cols, validation_frame = houses_data, 
                         model_id = "base_glrm", seed = 1234, 
                         k = glrm_k, gamma_x = gamma, gamma_y = gamma, 
                         regularization_x = "L1", regularization_y = "L1",
                         loss_by_col_idx = losses$index, loss_by_col = losses$loss)
  
  # Decompose training frame into XY
  X <- h2o.getFrame(rank2_model@model$representation_name)
  Y <- rank2_model@model$archetypes
  
  # Visualize first two archetypes of Y
  archetypes_y <- as.data.frame(t(Y))
  archetypes_y$attribute <- rownames(archetypes_y)
  
  # install.packages('plotly', dependencies = TRUE)
  library('plotly')
  library(stringr)
  m <- archetypes_y[archetypes_y$attribute %in% c("listingtypeentire_homeapt", "listingtypeprivate_room", "monoxid_bed_am" , "smok_fir_bed_am", "air_conditioning_bed_am"), ]
  m <- archetypes_y[archetypes_y$attribute %in% str_subset(archetypes_y$attribute, "smok"), ]

  a <- list(
    x = m$Arch1,
    y = m$Arch2,
    text = m$attribute,
    xref = "x",
    yref = "y"
  )
  plot_ly(data = archetypes_y, x = ~Arch1, y = ~Arch2, mode = "markers", text = ~attribute) %>% layout(annotations = a)
  
  # Visualize first two archetypes of X
  archetypes_x <- as.data.frame(X)
  archetypes_x$id <- as.character(as.matrix(H2O_data_for_GLRM$id))
  set.seed(1234)
  sample_indices <- sample(c(1:nrow(archetypes_x)), 100)
  archetypes_x <- archetypes_x[sample_indices, ]
  
  # Plot
  plot_ly(data = archetypes_x, x = ~Arch1, y = ~Arch2, mode = "markers", 
          text = ~paste0("Rental : ", pid))
}
