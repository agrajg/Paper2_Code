# NOTES : Fill NA for Complete sample in line 6
cat('# Begin Running file : ', '12_01_Create_Sample.R', " ...", '\n')
cat('# ------------------------------------------------------', '\n')

cat('# Taking a sample of rentals ... ', '\n')
{
  num.rentals = 100
  cat('# Number of rentals in sample = ', num.rentals, '\n')
}

if(!is.na(num.rentals)){ 
  cat('# Creating a sample of',num.rentals, 'rentals. ...', '\n')
  subfile.time <- Sys.time()
  propertyid_sample <- unique(demand.data$propertyid) %>% sample(size = num.rentals) # change this to vary the sample size.
  demand.data <- demand.data[propertyid %in% propertyid_sample]
  review.plain.data <- review.plain.data[propertyid %in% propertyid_sample]
  rental.data <- rental.data[propertyid %in% propertyid_sample]
  pid.data <- pid.data[propertyid %in% propertyid_sample]
  cat('# Begin Running file : ', '12_01_Create_Sample.R', " ...", '\n')
  cat('# Time taken : ', '\n')
  print(Sys.time() - subfile.time)
  rm(subfile.time)
  cat('# ======================================================', '\n')
}else{
  cat('# NO SAMPLING DONE .. ', '\n')
}


