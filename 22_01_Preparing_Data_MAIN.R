rm(list = ls())
source(file = "00_00_Preamble__current.R")
library(data.table)
library(quanteda)
library(parallel)

cat('* data.table options ...', '\n')
{
  setDTthreads(threads = detectCores())
  cat('* Data table threads set to ', getDTthreads(), '.', '\n')
}

cat('* quanteda options ... ', '\n')
{
  quanteda_options(threads = getDTthreads())
  quanteda_options(language_stemmer = 'English')
}

cat('* FILE : 11_01_Loading_Demand_Review_Rental_Data__current.R ... ', '\n')
source(file = "11_01_Loading_Demand_Review_Rental_Data__current.R")

cat('* FILE : 12_01_Create_Sample__current.R ... ', '\n')
source(file = "12_01_Create_Sample__current.R")

cat('* FILE : 22_01_Preparing_Reviews_Data.R ... ', '\n')
source(file = "22_01_Preparing_Reviews_Data.R")

# cat('* FILE : 22_01_Reduce_dimensionality_of_reviews_data.R ... ', '\n')
# source(file = "22_01_Reduce_dimensionality_of_reviews_data.R")
