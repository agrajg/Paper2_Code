cat('# Final demand panel prep and save.', '\n')
cat('# ---------------------------------', '\n')
{
  time <- Sys.time()
  cat('# Ordering ... ', '\n')
  pid.data <- pid.data %>% setDT()
  demand.data.temp <- pid.data[demand.data, on="propertyid"]                               # left join
  demand.data.temp <- demand.data.temp[order(propertyid, date)]
  {      
    cat('# Check consistancy ... ', '\n')
    cat('# Checking rows ... ', '\n')
    print(check_rows)
    cat('# Check propertyid ... ', '\n')
    print(demand.data.temp$propertyid[check_rows])
    cat('# Check date ... ', '\n')
    print(demand.data.temp$date[check_rows])
  }
  demand.data.temp <- demand.data.temp[ , !c("propertyid", "status", "reservationid","bookeddate", 
                           "hostid" , "createddate" , "state" , "neighborhood" ,  
                           "listingtype",	"nbhd",	"nbhd_group",	"borough",	"bedrooms",
                           "bathrooms",	"maxguests",	"latitude",	"longitude")]
  cat('# Writing the csv ... ', '\n')
  fwrite(x = demand.data.temp, 
         file = paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_Demand_panel_DT", ".csv", sep = ""),
         append = FALSE)
  rm(demand.data.temp)
  gc()
  cat('# Final demand panel prep and save.', '\n')
  cat('# Time taken : ', '\n')
  print(Sys.time() - time)
  cat('# =================================', '\n')
}