# ***********************************************
# sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
cat('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>', '\n')
cat('Begin Running file : ', "Data_setup.R", " ...", '\n')
# sink()
# ***********************************************


# In this file we prepare data in h2o. 
# Load data on H2o
# combine text data with demand data.
# Output a combined data will all the covariates.
# ---------------------------------------------------------------------------------------------------------------------
h2o.init(port = port.num, nthreads = n.threads, max_mem_size = max.mem, forceDL = force.DL)
h2o.removeAll() ## clean slate - just in case the cluster was already running
h2o.ls()


# Setting things in H2O environment and loading the entire data on to H2O environment.
# This may take about 20 minutes
# ---------------------------------------------------------------------------------------------------------------------
very.begin.time <- Sys.time()
ptm.loadH2Odata <- proc.time()
demand.data.h2o <- as.h2o(demand.data)
demand.data.h2o$date <- h2o.as_date(demand.data.h2o$date, format = "%d%b%Y")
proc.time() - ptm.loadH2Odata
h2o.ls()

ptm.loadH2Odata <- proc.time()
text.df.h2o <- as.h2o(text.df)
text.df.h2o$rev_date <- h2o.as_date(text.df.h2o$rev_date, format = "%d%b%Y")
proc.time() - ptm.loadH2Odata
h2o.ls()

ptm.loadH2Odata <- proc.time()
rental.time.panel.h2o <- as.h2o(rental.time.panel)
rental.time.panel.h2o$date <- h2o.as_date(rental.time.panel.h2o$date, format="%d%b%Y")
rental.time.panel.h2o$rev_date <- h2o.as_date(rental.time.panel.h2o$rev_date, format="%d%b%Y")
proc.time() - ptm.loadH2Odata
h2o.ls()
# =====================================================================================================================

# perform merge properly
# ---------------------------------------------------------------------------------------------------------------------
time <- Sys.time()
cat('Run begin ...', '\n')
m1 <- h2o.merge(rental.time.panel.h2o, text.df.h2o, by =  c("propertyid" , "rev_date"),all.x =  TRUE)
cat('SAMPLE : ' , '\n')
print(h2o.dim(m1))
print(m1[1:10,1:10])
h2o.ls()

m2 <- h2o.merge(rental.time.panel.h2o, text.df.h2o, by =  c("propertyid" , "rev_date"),all.y =  TRUE)
cat('SAMPLE : ' , '\n')
print(h2o.dim(m2))
print(m2[1:10,1:10])
h2o.ls()

mask <- is.na(m2[,"date"])
cols <- m2[mask,]
h2o.dim(cols)
tempdata.h2o <- h2o.rbind(m1, cols)
cat('SAMPLE : ' , '\n')
print(h2o.dim(tempdata.h2o))
print(tempdata.h2o[1:10,1:10])
h2o.ls()
cat('Run complete.', '\n')
cat('Time taken : ', '\n')
print(Sys.time() - time)
# Clean up 
h2o.rm(m1)
h2o.rm(m2)
h2o.rm(mask)
h2o.rm(cols)
h2o:::.h2o.garbageCollect()
h2o:::.h2o.garbageCollect()
h2o:::.h2o.garbageCollect()
h2o.ls()
# =====================================================================================================================

# Breaking the entire data into four parts to perform a sort
# ---------------------------------------------------------------------------------------------------------------------
time <- Sys.time()
cat('Run begin ...', '\n')
# text.panel.h2o <- h2o.arrange(tempdata.h2o , propertyid, date)
tempdata.h2o$date <- h2o.ifelse(is.na(tempdata.h2o$date), tempdata.h2o$rev_date, tempdata.h2o$date)
cat('SAMPLE : ' , '\n')
print(h2o.dim(tempdata.h2o))
print(tempdata.h2o[1:10,1:10])

mask <- tempdata.h2o[,"propertyid"] <= h2o.quantile(tempdata.h2o$propertyid, probs = c(0.25, 0.50, 0.75))[[1]]
print(mask)
tempdata.h2o.part1 <- tempdata.h2o[mask,]
cat('SAMPLE : ' , '\n')
print(h2o.dim(tempdata.h2o.part1))
print(tempdata.h2o.part1[1:10,1:10])
text.panel.h2o.part1 <- h2o.rank_within_group_by(tempdata.h2o.part1, 
                                                 group_by_cols = c("propertyid"), 
                                                 sort_cols =c("propertyid","date"), 
                                                 ascending = NULL,
                                                 new_col_name = "New_Rank_column", 
                                                 sort_cols_sorted = TRUE)


mask <- (tempdata.h2o[,"propertyid"] > h2o.quantile(tempdata.h2o$propertyid, probs = c(0.25, 0.50, 0.75))[[1]] & 
           tempdata.h2o[,"propertyid"] <= h2o.quantile(tempdata.h2o$propertyid, probs = c(0.25, 0.50, 0.75))[[2]])
print(mask)
tempdata.h2o.part2 <- tempdata.h2o[mask,]
cat('SAMPLE : ' , '\n')
print(h2o.dim(tempdata.h2o.part2))
print(tempdata.h2o.part2[1:10,1:10])
text.panel.h2o.part2 <- h2o.rank_within_group_by(tempdata.h2o.part2, 
                                                 group_by_cols = c("propertyid"), 
                                                 sort_cols =c("propertyid","date"), 
                                                 ascending = NULL,
                                                 new_col_name = "New_Rank_column", 
                                                 sort_cols_sorted = TRUE)


mask <- (tempdata.h2o[,"propertyid"] > h2o.quantile(tempdata.h2o$propertyid, probs = c(0.25, 0.50, 0.75))[[2]] & 
           tempdata.h2o[,"propertyid"] <= h2o.quantile(tempdata.h2o$propertyid, probs = c(0.25, 0.50, 0.75))[[3]])
print(mask)
tempdata.h2o.part3 <- tempdata.h2o[mask,]
cat('SAMPLE : ' , '\n')
print(h2o.dim(tempdata.h2o.part3))
print(tempdata.h2o.part3[1:10,1:10])
text.panel.h2o.part3 <- h2o.rank_within_group_by(tempdata.h2o.part3, 
                                                 group_by_cols = c("propertyid"), 
                                                 sort_cols =c("propertyid","date"), 
                                                 ascending = NULL,
                                                 new_col_name = "New_Rank_column", 
                                                 sort_cols_sorted = TRUE)


mask <- (tempdata.h2o[,"propertyid"] > h2o.quantile(tempdata.h2o$propertyid, probs = c(0.25, 0.50, 0.75))[[3]])
print(mask)
tempdata.h2o.part4 <- tempdata.h2o[mask,]
cat('SAMPLE : ' , '\n')
print(h2o.dim(tempdata.h2o.part4))
print(tempdata.h2o.part4[1:10,1:10])
text.panel.h2o.part4 <- h2o.rank_within_group_by(tempdata.h2o.part4, 
                                                 group_by_cols = c("propertyid"), 
                                                 sort_cols =c("propertyid","date"), 
                                                 ascending = NULL,
                                                 new_col_name = "New_Rank_column", 
                                                 sort_cols_sorted = TRUE)

text.panel.h2o <- h2o.rbind(text.panel.h2o.part1, text.panel.h2o.part2, text.panel.h2o.part3, text.panel.h2o.part4)

h2o.ls()
cat('SAMPLE : ' , '\n')
print(h2o.dim(text.panel.h2o))
print(text.panel.h2o[1:10,1:10])


h2o.rm(mask)
h2o.rm(tempdata.h2o)
h2o.rm(tempdata.h2o.part1)
h2o.rm(tempdata.h2o.part2)
h2o.rm(tempdata.h2o.part3)
h2o.rm(tempdata.h2o.part4)
h2o.rm(text.panel.h2o.part1)
h2o.rm(text.panel.h2o.part2)
h2o.rm(text.panel.h2o.part3)
h2o.rm(text.panel.h2o.part4)
h2o:::.h2o.garbageCollect()
h2o:::.h2o.garbageCollect()
h2o:::.h2o.garbageCollect()

h2o.ls()
cat('SAMPLE : ' , '\n')
print(h2o.dim(text.panel.h2o))
print(text.panel.h2o[1:10,1:10])

cat('Run complete.', '\n')
cat('Time taken : ', '\n')
print(Sys.time() - time)

# =====================================================================================================================

# Merging text date with the demand data
# ---------------------------------------------------------------------------------------------------------------------
text_colnames <- setdiff(h2o.colnames(text.panel.h2o),h2o.colnames(rental.time.panel.h2o))
text_colnames <- setdiff(text_colnames,c("New_Rank_column"))
first_text_col = text_colnames[[1]]

time <- Sys.time()
cat('Run begin ...', '\n')
# for(i in text_colnames){
#   text.panel.h2o[,i] = ifelse(text.panel.h2o[, "New_Rank_column"] ==1 & is.na(text.panel.h2o[,i]), 0, text.panel.h2o[,i])
# }

print(text.panel.h2o[11864:11869,1:10])

text.panel.h2o.part1 <- text.panel.h2o[, c("propertyid" , "rev_date" ,"date")] 
print(text.panel.h2o.part1[11864:11869,])

text.panel.h2o.part2 <- text.panel.h2o[,c(text_colnames, "New_Rank_column")] 
print(text.panel.h2o.part2[11864:11869,1:10])

text.panel.h2o.part2[,first_text_col] = ifelse(text.panel.h2o.part2[, "New_Rank_column"] ==1 & is.na(text.panel.h2o.part2[,first_text_col]), 0, text.panel.h2o.part2[,first_text_col])
print(text.panel.h2o.part2[11864:11869,1:10])


varnames <- h2o.colnames(text.panel.h2o.part2)
text.panel.h2o.part2 <- h2o.fillna(text.panel.h2o.part2, method = "forward", axis = 1, maxlen = 20000L)
print(text.panel.h2o.part2[11864:11869,1:10])
colnames(text.panel.h2o.part2) <- varnames
print(text.panel.h2o.part2[11864:11869,1:10])

text.panel.h2o <- h2o.cbind(text.panel.h2o.part1, text.panel.h2o.part2)
print(text.panel.h2o[11864:11869,1:10])
cat('SAMPLE : ' , '\n')
print(h2o.dim(text.panel.h2o))
print(text.panel.h2o[1:10,1:10])
cat('Run complete.', '\n')
cat('Time taken : ', '\n')
# h2o.rm(text.panel.h2o.part1)
# h2o.rm(text.panel.h2o.part2)
cat('SAMPLE : ' , '\n')
print(h2o.dim(text.panel.h2o))
print(text.panel.h2o[1:10,1:10])
h2o.ls()
print(Sys.time() - time)

# =====================================================================================================================

# Back filling the text data
# ---------------------------------------------------------------------------------------------------------------------
time <- Sys.time()
cat('Run begin ...', '\n')
varnames <- h2o.colnames(text.panel.h2o)
text.panel.h2o <- h2o.fillna(text.panel.h2o, method = "forward", axis = 2, maxlen = 1000L)
h2o.ls()
cat('SAMPLE : ' , '\n')
print(h2o.dim(text.panel.h2o))
print(text.panel.h2o[11864:11869,1:10])
colnames(text.panel.h2o) <- varnames
print(text.panel.h2o[11864:11869,1:10])

cat('Run complete.', '\n')
cat('Time taken : ', '\n')
print(Sys.time() - time)
# =====================================================================================================================
time <- Sys.time()
cat('Run begin ...', '\n')
final.df.h2o <- h2o.merge(rental.time.panel.h2o, demand.data.h2o, by =  c("propertyid" , "date"), all.x =  TRUE)
cat('SAMPLE : ' , '\n')
print(h2o.dim(final.df.h2o))
print(final.df.h2o[11864:11869,1:10])
# new_cols <- setdiff(h2o.colnames(final.df.h2o), c("rev_date", "status"))
# final.df.h2o <- final.df.h2o[,new_cols]
final.df.h2o <- h2o.merge(final.df.h2o, text.panel.h2o, by =  c("propertyid" , "date"), all.x =  TRUE)

cat('SAMPLE of the final data: ' , '\n')
cat('-------------------------------------------------------------------------------------------------------' , '\n')
print(h2o.dim(final.df.h2o))
print(final.df.h2o[11864:11869,1:10])
cat('-------------------------------------------------------------------------------------------------------' , '\n')

h2o.ls()
cat('Run complete.', '\n')
cat('Time taken : ', '\n')
print(Sys.time() - time)
# =====================================================================================================================
cat('Total time for data prep : ', '\n')
print(Sys.time() - very.begin.time)
# =====================================================================================================================


# ***********************************************
# sink(file = paste(project.path,"Output/Final/",output.filename, sep=""), append = TRUE)   # *** Keep in the final run ***
cat(' ... ', '\n')
cat('End Running file : ', "Data_setup.R", '\n')
cat('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<', '\n')
# sink()  # *** Keep in the final run ***
# ***********************************************
