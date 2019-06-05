# NOTES : Control the number of features on line 31-34.
rm(list = ls())
source(file = "00_00_Preamble__current.R")

cat('# Begin Running file : ', '21_01_Format_Reviews_Data.R', " ...", '\n')
cat('# ------------------------------------------------------------', '\n')
file.time <- Sys.time()

cat('# Packages needed in this program', '\n')
{
  library(slam)
  library(Matrix)
  library(ggplot2)
  library(foreach)
  library(doParallel)
  library(dplyr)
  library(data.table)
  library(stringr)
  library(magrittr)
  library(quanteda)
  library(tm)
  library(SnowballC)
  library(devtools)
  library(rlang)
}

cat('# data.table and quanteda options ...', '\n')
{
  # setDTthreads(threads = 0)
  cat('# Data table threads set to ', getDTthreads(), '.', '\n')
  quanteda_options(threads = detectCores() - 4)
  quanteda_options(language_stemmer = 'English')

  cat('# Setting up stopwords list ... ', '\n')
  language.list <- setdiff(getStemLanguages(),c("porter", "turkish"))
  stopwords.list <- unlist(c(lapply(X = language.list, FUN = stopwords)), recursive = TRUE, use.names = TRUE)
}

# Inputs
min.doc.percent <- 0.1    # Percentage of documents the term should atleast exist.

{
  cat('# FILE : 11_01_Loading_Demand_Review_Rental_Data__current.R ... ', '\n')
  source(file = "11_01_Loading_Demand_Review_Rental_Data__current.R")
  
  cat('# FILE : 12_01_Create_Sample__current.R ... ', '\n')
  source(file = "12_01_Create_Sample__current.R")
  
  if(length(list.files(path = paste(project.path, "Output/TEMP/ForH2O_DML/", sep = ""))) > 0){
    cat('# Removing existing files in the ForH2O_DML folder', '\n')
    file.remove(paste(project.path, 
                      "Output/TEMP/ForH2O_DML/",
                      list.files(path = paste(project.path, 
                                              "Output/TEMP/ForH2O_DML/", 
                                              sep = "")
                                 ),
                      sep = ""
                      )
                )
  }else{cat('# No files exist in ForH2O_DML folder', '\n')}
  
  cat('# FILE : 21_01_Configuring_review_text_data__current.R ... ', '\n')
  source(file = "21_01_Configuring_review_text_data__current.R")

  cat('# FILE : 21_01_Final_Demand_Data__current.R ... ', '\n')
  source(file = "21_01_Final_Demand_Data__current.R")
  
  cat('# FILE : 21_01_Final_Rental_Characteristic_Data__current.R ... ', '\n')
  source(file = "21_01_Final_Rental_Characteristic_Data__current.R")
}

cat('# End Running file : ', '03_01_Format_Reviews_Data.R', " ...", '\n')
cat('# Time taken : ', '\n')
print(Sys.time() - file.time)
cat('# =========================XXXXXXXX=========================', '\n')

