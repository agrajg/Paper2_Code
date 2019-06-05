# 03_01_Format_Reviews_Data

# Clearing up workspace
# ---------------------------------------------------------------------------------------------------------------------
rm(list = ls())
source(file = "00_00_Preamble.R")
# Packages needed in this program
# ---------------------------------------------------------------------------------------------------------------------
packages<-c("ggplot2", "foreach", "doParallel", "dplyr", "caret", "stringr", "gmm", "magrittr", "quanteda", "SnowballC", "tidytext")
# remove.packages(packages, lib = .libPaths())
check.packages(packages)
# =====================================================================================================================
# stopwords.list <- c(stopwords('en'),stopwords('fr'), stopwords('es'), stopwords('pt'), stopwords('de'), stopwords(language = "zh", source = "misc"), stopwords('ru'))
# stopwords.list <- c(stopwords('en'))
language.list <- setdiff(getStemLanguages(),c("porter", "turkish"))
stopwords.list <- (lapply(X = language.list, FUN = stopwords))
quanteda_options(threads = detectCores() - 1)
quanteda_options(language_stemmer = 'English')
# =====================================================================================================================



# # ***DON'T COMMENT IF RUNNING FRESH*** 
# # Creating a rental time panel
# # ---------------------------------------------------------------------------------------------------------------------
# ptm.PanelPrep <- proc.time()
# load(file = paste(project.path,"Output/TEMP/","11_00_Demand_Regression_data.RData", sep=""))
# gc()
# rental.time.panel <-  demand.data %>% 
#   select(propertyid, date)
# save(rental.time.panel, file = paste(project.path, "Output/TEMP/", "21_01_rental_time_panel.RData", sep = ""))
# rm(demand.data)
# rm(rental.time.panel)
# gc()
# cat('Time taken to prepare the panel : ')
# print(proc.time() - ptm.PanelPrep)
# # =====================================================================================================================
# load(file = paste(project.path,"Output/TEMP/","21_01_rental_time_panel.RData", sep=""))
# rental.time.panel$date <- as.Date(rental.time.panel$date, "%d%b%Y")


start.loadReview <- Sys.time()
load(file = paste(project.path,"Output/TEMP/","11_00_Reviews_data.RData", sep=""))
review.data$date <- as.Date(review.data$date, "%d%b%Y")
end.loadReview <- Sys.time()
cat('Time take to load reviews data is : ')
print(end.loadReview - start.loadReview)
gc()

# review.panel <- rental.time.panel %>% left_join(review.data, by = c("propertyid", "date"))
# rm(rental.time.panel)
# rm(review.data)


# # Take a sample (this part need to be commented out)
# # ---------------------------------------------------------------------------------------------------------------------
# # demand.data.2 <- demand.data
# set.seed(48374)
# review.plain.data <- sample_frac(review.plain.data, size = 0.02)
# # =====================================================================================================================


review.tokens <- tokens(review.data$corpus, 
                       what = "word", 
                       remove_numbers = TRUE, 
                       remove_punct = TRUE,
                       remove_symbols = TRUE, 
                       remove_hyphens = TRUE, 
                       remove_twitter = TRUE, 
                       remove_url = TRUE, 
                       remove_separators = TRUE)


review.tokens[[357]]
review.tokens <- tokens_tolower(review.tokens)
review.tokens[[357]]
review.tokens <- tokens_select(review.tokens, pattern = stopwords('English'), selection = "remove")
review.tokens[[357]]
review.tokens <- tokens_wordstem(review.tokens, language = quanteda_options("language_stemmer"))
review.tokens[[357]]
# rm(list = c("language.list", "stopwords.list"))
# gc()


review.tokens.dfm <- dfm(review.tokens, tolower = FALSE)


DF <- tidy(review.tokens.dfm)


DF2 <- DF %>% cast_dtm(document, term, count)

# convert(review.tokens.dfm, to = c("matrix"), docvars = NULL)


# review.tokens.df <- as.data.frame(review.tokens.dfm)
colnames(review.tokens.df)
# convert(review.tokens.dfm, to = "data.frame")
