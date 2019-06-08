# ***********************************************
# sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
cat('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>', '\n')
cat('Begin Running file : ', "Take_a_sample.R", " ...", '\n')
# sink()
# ***********************************************


# # Filter by date
# # ---------------------------------------------------------------------------------------------------------------------
# demand.data <- demand.data %>% 
#   filter(as.Date(date, format = "%d%b%Y") < as.Date("2015-01-01"))
# text.df <- text.df %>% 
#   filter(as.Date(rev_date, format = "%d%b%Y") < as.Date("2015-01-01"))
# rental.time.panel <- rental.time.panel %>% 
#   filter(as.Date(date, format = "%d%b%Y") < as.Date("2015-01-01"))
# 
# Filter by propertyid 
# ---------------------------------------------------------------------------------------------------------------------
demand.data <- demand.data %>%
  filter(propertyid >= 0 & propertyid <= 50000)
text.df <- text.df %>%
  filter(propertyid >= 0 & propertyid <= 50000)
rental.time.panel <- rental.time.panel %>%
  filter(propertyid >= 0 & propertyid <= 50000)

# ***********************************************
# sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
cat('SAMPLED DATA :', '\n')
cat('--------------', '\n')
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

cat(' ... ', '\n')
cat('End Running file : ', "DML_Function.R", '\n')
cat('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<', '\n')
# sink()  # *** Keep in the final run ***
# ***********************************************

