{
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
