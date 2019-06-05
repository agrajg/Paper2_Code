cat('Initializing ML performance and DML results lists etc. ... ','\n')
{
  stats.name <- c("beta", "se")
  results.list.names <- apply(expand.grid(stats.name, D), 1, paste, collapse=".")
  Results_DML <- vector("list", length = 5+length(results.list.names))
  names(Results_DML) <- c("DML_iteration", "DML_fold_seed", "DML_type", "K_folds", "method_name", results.list.names)
  cat('* Initial Results list : ', '\n')
  print(Results_DML)

  ML_perf_Results <- list(var_label = c(),
                          DML_iteration = c(),
                          DML_fold_seed = c(),
                          target_name = c(), 
                          method_name = c(), 
                          train_MSE = c(), 
                          xval_MSE = c(),
                          parameters = c(),
                          seed = c())  
}



cat('* Output ML performance and DML results - ', i , ' - time.','\n')
{
  ML_perf_Results <- ML_perf_Results %>% as.data.frame() %>% setDT()
  Results_DML <-  Results_DML %>% as.data.frame() %>% setDT()
  cat('* ML Algoritm Performance data frame ... ', '\n')
  ML_perf_Results %>% print()
  cat('* DML Results data frame ... ', '\n')
  Results_DML %>% print()
  
  cat('* Write ML Algoritm Performance results to a file ... ', '\n')
  {
    if(i==1){
      fwrite(ML_perf_Results , file =  paste(project.path, "Output/Final/", "50_00_ML_perf_Results", ".csv", sep = ""), append = FALSE)
      fwrite(Results_DML , file =  paste(project.path, "Output/Final/", "50_00_ML_perf_Results", ".csv", sep = ""), append = FALSE)
    }else{
      fwrite(ML_perf_Results , file =  paste(project.path, "Output/Final/", "50_00_ML_perf_Results", ".csv", sep = ""), append = TRUE)
      fwrite(Results_DML , file =  paste(project.path, "Output/Final/", "50_00_ML_perf_Results", ".csv", sep = ""), append = TRUE)
    }  
  }
}



