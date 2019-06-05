# NOTES : 
# NOTES : Control the number of features on line 31-34.



cat('* R setup', '\n')
cat('* -------', '\n')
{
  rm(list = ls())
  source(file = "00_00_Preamble.R")
  # Packages needed in this program
  packages<-c("slam", "Matrix","ggplot2", "foreach", "doParallel", "tidyr", "dplyr", "data.table", "stringr", 
              "gmm", "magrittr", "quanteda", "tm", "SnowballC", "devtools", "rlang")
  # remove.packages(packages, lib = .libPaths())
  check.packages(packages)
  {
    cat('* data.table options ...', '\n')
    setDTthreads(threads = detectCores() - 2)
    cat('* Data table threads set to ', getDTthreads(), '.', '\n')
  }
  {
    cat('* quanteda options ... ', '\n')
    quanteda_options(threads = detectCores() - 2)
    quanteda_options(language_stemmer = 'English')
  }
  file.time <- Sys.time()
}


cat('* Read Inputs ... ', '\n')
{
  min_docfreq.prop <- c(0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005)[3]
}

cat('* Setting up stopwords list ... ', '\n')
{
  # stopwords.list <- c(stopwords('en'),stopwords('fr'), stopwords('es'), stopwords('pt'), stopwords('de'), stopwords(language = "zh", source = "misc"), stopwords('ru'))
  # stopwords.list <- c(stopwords('en'))
  language.list <- setdiff(getStemLanguages(),c("porter", "turkish"))
  stopwords.list <- unlist(c(lapply(X = language.list, FUN = stopwords)), recursive = TRUE, use.names = TRUE)
}


cat('* Loading and preparing the rental data', '\n')
cat('* -------------------------------------', '\n')
{
  time <- Sys.time()
  rental.data <- readRDS(file = paste(project.path,"Output/TEMP/","01_01_Rental_Characteristics_Add_Desc.rds", sep=""))
  cat('* Structure of rental.data : ', '\n')
  print(str(rental.data))
  cat('* Loading and preparing the rental data', '\n')
  cat('* Time taken : ', '\n')
  print(Sys.time() - time)
  cat('* =====================================', '\n')
}
