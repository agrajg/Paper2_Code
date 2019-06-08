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
  
  Z <- setdiff(str_subset(final_df_h2o %>% h2o.colnames(), "prod_week") , c("proddum",Z_alt))
  
  cat('****************** Change Z specification ******************' , '\n')
  cat('* Change Z to Z_alt ... ', '\n')
  {
    Z <- Z_alt
  }
  
  cat('* X ...','\n')
  X <- c(X_numeric, X_factors, X_date, X_from_text)
  
  cat('* Defining variables.', '\n')
  cat('* ===================', '\n')
}

# # Remove next chunk the final program
# cat('* Taking a subset of columns ... ', '\n')
# cat('* This portion was supposed to be commented out in the Final program !!!!!!!!!!!!!!!!!!!!!!!!!!!', '\n')
# {
#   Y <- c("qdemand")                                                                                # Remove in the final program
#   D <- c("lprice_per_person", "lprice")                                                            # Remove in the final program
#   Z <- Z[1:4]                                                                                      # Remove in the final program
#   cat('* This portion was supposed to be commented out in the Final program !!!!!!!!!!!!!!!!!!!!!!!!!!!!!', '\n')
# }

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
  cat('* X : ', '\n')
  cat('* ------------------------------', '\n')
  print(X)
  cat('* ==============================', '\n')
}    

# cat('* Remove Clutter ... ', '\n')
# final_df_h2o <- final_df_h2o[, c(Y,D,Z,X)]

cat('* Final data check ', '\n')
cat('-------------------' , '\n')
{
  cat('* DIM of the final data : ' , '\n')
  print(h2o.dim(final_df_h2o))
  cat('* SAMPLE of the final data : ' , '\n')
  print(final_df_h2o[11864:11869,1:10])
  cat('* STR of the final data : ')
  h2o.str(final_df_h2o)
  h2o.assign(final_df_h2o, "final_df_h2o")
  cat('* H2O Objects after data is setup for DML: ', '\n')
  print(h2o.ls())
  cat('* Final data check ', '\n')
  cat('* ================' , '\n')
}
