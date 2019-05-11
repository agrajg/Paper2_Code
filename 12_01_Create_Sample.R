cat('* Begin Running file : ', '12_01_Create_Sample.R', " ...", '\n')
cat('* ------------------------------------------------------', '\n')
{ 
  subfile.time <- Sys.time()
  propertyid_sample <- unique(demand.data$propertyid) %>% sample(size = 100)
  demand.data <- demand.data[propertyid %in% propertyid_sample]
  review.plain.data <- review.plain.data[propertyid %in% propertyid_sample]
  cat('* Begin Running file : ', '12_01_Create_Sample.R', " ...", '\n')
  cat('* Time taken : ', '\n')
  print(Sys.time() - subfile.time)
  rm(subfile.time)
  cat('* ======================================================', '\n')
}


