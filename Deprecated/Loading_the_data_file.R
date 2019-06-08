cat('Begin Running file : ', "Loading_the_data_file.R", " ...", '\n')
cat('--------------------------------------------------------', '\n')
file.time <- Sys.time()
# Demand data
demand.data <- readRDS(file = paste(project.path,"Output/TEMP/","01_01_Demand_Regression_data.rds", sep=""))
# Reviews data
load(file = paste(project.path,"Output/TEMP/","22_01_text_df.RData", sep=""))
# Create rental time panel
rental.time.panel <-  demand.data %>% 
  mutate(rev_date = date) %>%                   # mutate(rev_date = either booking date or date of stay)
  select(propertyid, date, rev_date) %>%
  as.tbl()

# ***********************************************
# sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
cat('Demand data :', '\n')
cat('-------------------------------------------------', '\n')
print(demand.data %>% str())
cat('=================================================', '\n')

cat('Text data :', '\n')
cat('-------------------------------------------------', '\n')
print(text.df %>% str())
cat('=================================================', '\n')

cat('Demand data :', '\n')
cat('-------------------------------------------------', '\n')
print(rental.time.panel %>% str())
cat('=================================================', '\n')
gc()
cat(' ... ', '\n')
cat('End Running file : ', "Loading_the_data_file.R", '\n')
cat('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<', '\n')
# sink()
# ***********************************************
