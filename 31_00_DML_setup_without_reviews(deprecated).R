# Clearing up workspace
# ---------------------------------------------------------------------------------------------------------------------
rm(list = ls())
source(file = "00_00_Preamble.R")

# Packages needed in this program
# ---------------------------------------------------------------------------------------------------------------------
packages<-c("ggplot2", "foreach", "doParallel", "dplyr", "caret", "h2o", "parallel", "lubridate", "recipes")
check.packages(packages)

# Initialize a cluster
# ---------------------------------------------------------------------------------------------------------------------
cl <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cl)

# Loading the data file 
# ---------------------------------------------------------------------------------------------------------------------
load(file = paste(project.path,"Output/TEMP/","11_00_Demand_Regression_data.RData", sep=""))

# Take a sample (this part need to be commented out)
# ---------------------------------------------------------------------------------------------------------------------
demand.data.2 <- demand.data
demand.data <- sample_frac(demand.data, size = 0.001)


# Making data nice and ready
# ---------------------------------------------------------------------------------------------------------------------
# Y
demand.data$qdemand <- as.numeric(demand.data$qdemand)

# D
demand.data$lprice_per_person <- as.numeric(demand.data$lprice_per_person)

# Z
demand.data$prod_week1 <- as.factor(demand.data$prod_week1)
demand.data$prod_week2 <- as.factor(demand.data$prod_week2)
demand.data$prod_week3 <- as.factor(demand.data$prod_week3)
demand.data$prod_week4 <- as.factor(demand.data$prod_week4)
demand.data$prod_week5 <- as.factor(demand.data$prod_week5)

# X
demand.data$qdemand_l1 <- as.numeric(demand.data$qdemand_l1)
demand.data$qdemand_l2 <- as.numeric(demand.data$qdemand_l2)
demand.data$qdemand_l3 <- as.numeric(demand.data$qdemand_l3)

demand.data$listingtype <- as.factor(demand.data$listingtype)   
demand.data$bedrooms <- as.factor(demand.data$bedrooms) 
demand.data$bathrooms <- as.factor(demand.data$bathrooms) 
demand.data$nbhd <- as.factor(demand.data$nbhd)
demand.data$latitude <- as.numeric(demand.data$latitude)
demand.data$longitude <-  as.numeric(demand.data$longitude)
demand.data$p_age <- as.numeric(demand.data$p_age)
demand.data$h_age <- as.numeric(demand.data$h_age)
demand.data$p_dayshosting <- as.numeric(demand.data$p_dayshosting)
demand.data$h_dayshosting <- as.numeric(demand.data$h_dayshosting)
demand.data$p_daysbooked <- as.numeric(demand.data$p_daysbooked)
demand.data$h_daysbooked <- as.numeric(demand.data$h_daysbooked)
demand.data$p_guestcount <- as.numeric(demand.data$p_guestcount)
demand.data$h_guestcount <- as.numeric(demand.data$h_guestcount)
# Rental ID
demand.data$propertyid <- as.factor(demand.data$propertyid)
# handling date variables
demand.data$date <- as.Date(demand.data$date, format="%d%b%Y")
demand.data$week <- as.factor(week(demand.data$date))
demand.data$year <- as.factor(year(demand.data$date))
demand.data$wday <- as.factor(wday(demand.data$date))
demand.data$date <- as.factor(demand.data$date)




# Defining input output and other parameters
# ---------------------------------------------------------------------------------------------------------------------
# Y
y <- c("qdemand")

# D
d <- c("lprice_per_person")

# Z
z <- c("prod_week1", "prod_week2", "prod_week3", "prod_week4", "prod_week5")

# X
x <- c("qdemand_l1","qdemand_l2" ,"qdemand_l3", 
       "listingtype", "bedrooms", "bathrooms", "nbhd",
       "latitude","longitude", 
       "p_age", "h_age" ,"p_dayshosting" ,"h_dayshosting" ,
       "p_daysbooked" ,"h_daysbooked", 
       "p_guestcount", "h_guestcount",
       "propertyid", "date")



rec_yx <- recipe(qdemand ~ qdemand_l1 + qdemand_l2 + qdemand_l3 + listingtype + bedrooms + bathrooms + nbhd 
                  + latitude + longitude + p_age + h_age + p_dayshosting + h_dayshosting + p_daysbooked 
                  + h_daysbooked + p_guestcount + h_guestcount + propertyid + date, data = demand.data)

summary(rec_yx)

glmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)
glmnet
# Procedure

data <- demand.data
K = 10


split     <- runif(nrow(data), min = 0, max = 1)
cvgroup   <- as.numeric(cut(split,quantile(split,probs = seq(0, 1, 1/K)),include.lowest = TRUE))
# for(j in 1:K) {
j=1
  ii  <- cvgroup == j
  nii <- cvgroup != j
  if(K==1){
    ii  <- cvgroup == j
    nii <- cvgroup == j
  }
  datause <- data[nii,]
  cat("Dimension of data in use : ", dim(datause))
  dataout <- data[ii,]
  cat("Dimension of data held out: ", dim(dataout))
  
  cat("Splitting data in use into training and validation data ...")
  set.seed(3456)
  trainIndex <- createDataPartition(datause$y, p = .8, 
                                    list = FALSE, 
                                    times = 1)
  head(trainIndex)
  datause.train <- datause[ trainIndex,]
  datause.test  <- datause[-trainIndex,]
  
  
  
  fit.glm <- train(y=datause.train[,y], 
                   x=datause.train[,c("lprice_per_person", "p_guestcount")],
                   data=datause.train,
                   method = "glm")
  
  
# }
# stopCluster(cl)

