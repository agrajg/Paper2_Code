{
  cat('# Load data on H2O','\n')
  # h2o.removeAll()
  
  Data_Set_List <- c()

  {
    if(file.exists(paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_Demand_panel_DT", ".csv", sep = ""))){
      cat('# File : ' , paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_Demand_panel_DT", ".csv", sep = "") ,' EXISTS.', '\n')
      assign("Demand_panel_DT" , h2o.importFile(path = paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_Demand_panel_DT", ".csv", sep = ""), 
                                                destination_frame = "Demand_panel_DT", 
                                                header = TRUE))
      cat('# Dimension of this data : ', '\n')
      print(h2o.dim(get("Demand_panel_DT")))
      cat('# Add to the list Data_Set_List ... ', '\n')
      Data_Set_List <- c(Data_Set_List, get("Demand_panel_DT"))
      cat('# ^Importing done. ', '\n')
    }  
    print(h2o.ls())
    
    for(i in seq(from = 1, to = max_text_features, by = 200)){
      if(file.exists(paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_text_panel_DT_", as.character(i), ".csv", sep = ""))){
        cat('# File : ' , paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_text_panel_DT_", as.character(i), ".csv", sep = "") ,' EXISTS.', '\n')
        assign(paste("text_panel_DT_", as.character(i), sep = ""), 
               h2o.importFile(path = paste(project.path, 
                                           "Output/TEMP/ForH2O_DML/", 
                                           "21_01_text_panel_DT_", 
                                           as.character(i), 
                                           ".csv", 
                                           sep = ""), 
                              destination_frame = paste("text_panel_DT_", as.character(i), sep = ""), 
                              header = TRUE))
        
        cat('# Dimension of this data : ', '\n')
        print(h2o.dim(get(paste("text_panel_DT_", as.character(i), sep = ""))))
        cat('# Add to the list Data_Set_List ... ', '\n')
        Data_Set_List <- c(Data_Set_List, get(paste("text_panel_DT_", as.character(i), sep = "")))
        cat('# ^Importing done. ', '\n')
        h2o.ls()
      }else{
        cat('# File : ' , paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_text_panel_DT_", as.character(i), ".csv", sep = "") ,' DOES NOT EXIST.', '\n')
        break
      }
    }
    print(h2o.ls())
    
    
    if(file.exists(paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_allchar_panel_DT", ".csv", sep = ""))){
      cat('# File : ' , paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_allchar_panel_DT", ".csv", sep = "") ,' EXISTS.', '\n')
      assign(paste("allchar_panel_DT", sep = ""), 
             h2o.importFile(path = paste(project.path, 
                                         "Output/TEMP/ForH2O_DML/", 
                                         "21_01_allchar_panel_DT", 
                                         ".csv", 
                                         sep = ""), 
                            destination_frame = paste("allchar_panel_DT", sep = ""), 
                            header = TRUE))
      
      cat('# Dimension of this data : ', '\n')
      print(h2o.dim(get(paste("allchar_panel_DT", sep = ""))))
      cat('# Add to the list Data_Set_List ... ', '\n')
      Data_Set_List <- c(Data_Set_List, get(paste("allchar_panel_DT", sep = "")))
      cat('# ^Importing done. ', '\n')
      h2o.ls()
    }else{
      cat('# File : ' , paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_allchar_panel_DT", ".csv", sep = "") ,' DOES NOT EXIST.', '\n')
      break
    }
    print(h2o.ls())
    
    
    for(i in seq(from = 1, to = max_text_features, by = 200)){
      if(file.exists(paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_bed_amenities_panel_DT_", as.character(i), ".csv", sep = ""))){
        cat('# File : ' , paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_bed_amenities_panel_DT_", as.character(i), ".csv", sep = "") ,' EXISTS.', '\n')
        assign(paste("bed_amenities_panel_DT_", as.character(i), sep = ""), 
               h2o.importFile(path = paste(project.path, 
                                           "Output/TEMP/ForH2O_DML/", 
                                           "21_01_bed_amenities_panel_DT_", 
                                           as.character(i), 
                                           ".csv", 
                                           sep = ""), 
                              destination_frame = paste("bed_amenities_panel_DT_", as.character(i), sep = ""), 
                              header = TRUE))
        
        cat('# Dimension of this data : ', '\n')
        print(h2o.dim(get(paste("bed_amenities_panel_DT_", as.character(i), sep = ""))))
        cat('# Add to the list Data_Set_List ... ', '\n')
        Data_Set_List <- c(Data_Set_List, get(paste("bed_amenities_panel_DT_", as.character(i), sep = "")))
        cat('# ^Importing done. ', '\n')
        h2o.ls()
      }
      else{
        cat('# File : ' , paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_bed_amenities_panel_DT_", as.character(i), ".csv", sep = "") ,' DOES NOT EXIST.', '\n')
        break
      }
    }
    print(h2o.ls())
    
    
    for(i in seq(from = 1, to = max_text_features, by = 200)){
      if(file.exists(paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_desc_panel_DT_", as.character(i), ".csv", sep = ""))){
        cat('# File : ' , paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_desc_panel_DT_", as.character(i), ".csv", sep = "") ,' EXISTS.', '\n')
        assign(paste("desc_panel_DT_", as.character(i), sep = ""), 
               h2o.importFile(path = paste(project.path, 
                                           "Output/TEMP/ForH2O_DML/", 
                                           "21_01_desc_panel_DT_", 
                                           as.character(i), 
                                           ".csv", 
                                           sep = ""), 
                              destination_frame = paste("desc_panel_DT_", as.character(i), sep = ""), 
                              header = TRUE))
        
        cat('# Dimension of this data : ', '\n')
        print(h2o.dim(get(paste("desc_panel_DT_", as.character(i), sep = ""))))
        cat('# Add to the list Data_Set_List ... ', '\n')
        Data_Set_List <- c(Data_Set_List, get(paste("desc_panel_DT_", as.character(i), sep = "")))
        cat('# ^Importing done. ', '\n')
        h2o.ls()
      }
      else{
        cat('# File : ' , paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_desc_panel_DT_", as.character(i), ".csv", sep = "") ,' DOES NOT EXIST.', '\n')
        break
      }
    }
    print(h2o.ls())
    
    
    cat('# Length of the list containing all the data : ', length(Data_Set_List), '\n')
    cat('Binding data by columns' , '\n')
    final_df_h2o <- h2o.assign(do.call("h2o.cbind", args = Data_Set_List), key = "final_df_h2o")
    cat('# Final data dimensions : ', '\n')
    print(h2o.dim(final_df_h2o))
  }
  
  cat('# Creating relevant variables for DML.', '\n')
  {
    cat('# Making data nice and ready ... ', '\n')
    cat('# Y ...','\n')
    final_df_h2o$qdemand <- as.numeric(final_df_h2o$qdemand)
    
    cat('# D ...','\n')
    final_df_h2o$lprice_per_person <- as.numeric(final_df_h2o$lprice_per_person)
    
    cat('# Z ...','\n')
    final_df_h2o$prod_week1 <- as.numeric(final_df_h2o$prod_week1)
    final_df_h2o$prod_week2 <- as.numeric(final_df_h2o$prod_week2)
    final_df_h2o$prod_week3 <- as.numeric(final_df_h2o$prod_week3)
    final_df_h2o$prod_week4 <- as.numeric(final_df_h2o$prod_week4)
    final_df_h2o$prod_week5 <- as.numeric(final_df_h2o$prod_week5)

    cat('# Making Dummy variables production dummy ...','\n')
    for(pvar in c("prod_week1", "prod_week2", "prod_week3", "prod_week4", "prod_week5")) {
    #   temp.list = h2o.table(final_df_h2o[, pvar])[, pvar] %>% as.vector()
    #   cat('# Making Dummy variables production dummy ...','\n')
    #   for(i in temp.list) {
    #     cat('# Creating variable : ', paste(pvar, as.character(i), sep = "_") ,'\n')
    #     final_df_h2o[ , paste(pvar, as.character(i), sep = "_")] = h2o.ifelse(final_df_h2o[, pvar] == i, 1, 0)
    #     cat('# Creating variable : ', paste(pvar, as.character(i), "cap", sep = "_") ,'\n')
    #     final_df_h2o[ , paste(pvar, as.character(i), "cap", sep = "_")] = h2o.ifelse(final_df_h2o[, pvar] == i, final_df_h2o[, "capacity"], 0)
    #     final_df_h2o[ , paste(pvar, as.character(i), sep = "_")] = h2o.asfactor(final_df_h2o[ , paste(pvar, as.character(i), sep = "_")])
    #   }
      cat('# Creating variable : ',paste(pvar, "contcap", sep = "_") ,'\n')
      final_df_h2o[, paste(pvar, "contcap", sep = "_")] = final_df_h2o[, pvar]*final_df_h2o[, "capacity"]
    }
    
    cat('# X ...', '\n')
    
    cat('# Numeric covariates', '\n')
    X_numeric <- c("qdemand_l1","qdemand_l2","qdemand_l3",
                   "p_age", "h_age","p_dayshosting","h_dayshosting",
                   "p_daysbooked","h_daysbooked","p_guestcount","h_guestcount",
                   "host_response_rate","host_acceptance_rate","security_deposit","cleaning_fee",
                   "guests_included","extra_people","minimum_nights","maximum_nights",
                   "review_scores_rating","review_scores_accuracy","review_scores_cleanliness","review_scores_checkin",
                   "review_scores_communication", "review_scores_location","review_scores_value",
                   "host_listing_count",
                   "thumbnail_url_dum","medium_url_dum","picture_url_dum","xl_picture_url_dum",
                   "host_url_dum","host_thumbnail_url_dum","host_picture_url_dum",
                   "host_verifications_email","host_verifications_phone","host_verifications_facebook","host_verifications_linkedin", 
                   "host_verifications_reviews","host_verifications_gid","host_verifications_others",
                   "host_is_superhost_dum","host_has_profile_pic_dum","host_identity_verified_dum","requires_license_dum","rgpv_dum","rgpp_dum",
                   "bedrooms","bathrooms","maxguests","securitydeposit","cleaningfee","extrapeoplefee","minimumstay","latitude","longitude"
                   )
    for(x in X_numeric){
      final_df_h2o[[x]] <- h2o.asnumeric(final_df_h2o[[x]])
    }
    
    cat('# Factor covariates', '\n')
    X_factors <- c("host_response_time",
                   "propertytype","listingtype","cancellationpolicy","checkintime","checkouttime","instantbookenabled",
                   "nbhd","nbhd_group","borough"
                   )
    for(x in X_factors){
      final_df_h2o[[x]] <- h2o.asfactor(final_df_h2o[[x]])
    }

    cat('# Date covariates', '\n')
    final_df_h2o$week <- as.factor(h2o.week(final_df_h2o$date))
    print(h2o.table(final_df_h2o$week))
    final_df_h2o$year <- as.factor(h2o.year(final_df_h2o$date))
    print(h2o.table(final_df_h2o$year))
    final_df_h2o$dayOfWeek <- as.factor(h2o.dayOfWeek(final_df_h2o$date))
    print(h2o.table(final_df_h2o$dayOfWeek))
    final_df_h2o$date <- as.factor(as.numeric(final_df_h2o$date))
    X_date <- c("week", "year", "dayOfWeek", "date")    
     
    cat('# X from text', '\n')
    X_from_text <- c(str_subset(h2o.colnames(final_df_h2o), "_text") ,
                     str_subset(h2o.colnames(final_df_h2o), "_bed_amenities") , 
                     str_subset(h2o.colnames(final_df_h2o), "_desc")
    )
    
    cat('# Rental ID ...','\n')
    final_df_h2o$pid <- as.factor(final_df_h2o$pid)
  }
}

