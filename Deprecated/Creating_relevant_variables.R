# ***********************************************
# sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
cat('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>', '\n')
cat('Begin Running file : ', "Creating_relevant_variables.R", " ...", '\n')
# sink()
# ***********************************************


# Creating relevant variables for DML

print(final.df.h2o[1:10,1:10])

# Making data nice and ready
# ---------------------------------------------------------------------------------------------------------------------
# Y
final.df.h2o$qdemand <- as.numeric(final.df.h2o$qdemand)
# D
final.df.h2o$lprice_per_person <- as.numeric(final.df.h2o$lprice_per_person)

# Z
final.df.h2o$prod_week1 <- as.numeric(final.df.h2o$prod_week1)
final.df.h2o$prod_week2 <- as.numeric(final.df.h2o$prod_week2)
final.df.h2o$prod_week3 <- as.numeric(final.df.h2o$prod_week3)
final.df.h2o$prod_week4 <- as.numeric(final.df.h2o$prod_week4)
final.df.h2o$prod_week5 <- as.numeric(final.df.h2o$prod_week5)

for(pvar in c("prod_week1", "prod_week2", "prod_week3", "prod_week4", "prod_week5")) {
  temp.list = h2o.table(final.df.h2o[, pvar])[, pvar] %>% as.vector()
  for(i in temp.list) {
    final.df.h2o[ , paste(pvar, as.character(i), sep = "_")] = h2o.ifelse(final.df.h2o[, pvar] == i, 1, 0)
    final.df.h2o[ , paste(pvar, as.character(i), "cap", sep = "_")] = h2o.ifelse(final.df.h2o[, pvar] == i, final.df.h2o[, "capacity"], 0)
  }
  final.df.h2o[, paste(pvar, "contcap", sep = "_")] = final.df.h2o[, pvar]*final.df.h2o[, "capacity"]
}

# X
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

# Rental ID
final.df.h2o$propertyid <- as.factor(final.df.h2o$propertyid)

# handling date variables
final.df.h2o$week <- as.factor(h2o.week(final.df.h2o$date))
final.df.h2o$year <- as.factor(h2o.year(final.df.h2o$date))
final.df.h2o$dayOfWeek <- as.factor(h2o.dayOfWeek(final.df.h2o$date))
final.df.h2o$date <- as.factor(final.df.h2o$date)
# =====================================================================================================================

# Defining input output and other parameters
# ---------------------------------------------------------------------------------------------------------------------
# Y
Y <- c("qdemand")
# D
D <- c("lprice_per_person")
# Z
Z1 <- c("prod_week1", "prod_week2", "prod_week3", "prod_week4", "prod_week5", 
        "prod_week1_contcap", "prod_week2_contcap", "prod_week3_contcap", "prod_week4_contcap", "prod_week5_contcap")

Z <- setdiff(str_subset(final.df.h2o %>% h2o.colnames(), "prod_week") , c("proddum",Z1))
# X
X1 <- c("qdemand_l1","qdemand_l2" ,"qdemand_l3", 
        "listingtype", "bedrooms", "bathrooms", "nbhd",
        "latitude","longitude", 
        "p_age", "h_age" ,"p_dayshosting" ,"h_dayshosting" ,
        "p_daysbooked" ,"h_daysbooked", 
        "p_guestcount", "h_guestcount",
        "propertyid", "date", "year", "week", "dayOfWeek")
X2 <- str_subset(final.df.h2o %>% h2o.colnames(), "_textvar")
X <- c(X1,X2)
cat('==============================', '\n')
cat('Y : ', '\n')
cat('------------------------------', '\n')
print(Y)
cat('D : ', '\n')
cat('------------------------------', '\n')
print(D)
cat('Z : ', '\n')
cat('------------------------------', '\n')
print(Z)
cat('X : ', '\n')
cat('------------------------------', '\n')
print(X)
cat('==============================', '\n')

# =====================================================================================================================

# Remove Clutter
# ---------------------------------------------------------------------------------------------------------------------
final.df.h2o <- final.df.h2o[, c(Y,D,Z,Z1,X)]

# Final data check
# ---------------------------------------------------------------------------------------------------------------------
cat('SAMPLE of the final data: ' , '\n')
cat('-------------------------------------------------------------------------------------------------------' , '\n')
print(h2o.dim(final.df.h2o))
print(final.df.h2o[11864:11869,1:10])
h2o.str(final.df.h2o)
cat('-------------------------------------------------------------------------------------------------------' , '\n')

h2o.ls()
cat('Run complete.', '\n')
cat('Time taken : ', '\n')
print(Sys.time() - time)
# =====================================================================================================================
cat('Total time for data prep : ', '\n')
print(Sys.time() - very.begin.time)
# =====================================================================================================================


# =====================================================================================================================
# ***********************************************
# sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
cat(' ... ', '\n')
cat('End Running file : ', "Creating_relevant_variables.R", '\n')
cat('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<', '\n')
# sink()  # *** Keep in the final run ***
# ***********************************************
