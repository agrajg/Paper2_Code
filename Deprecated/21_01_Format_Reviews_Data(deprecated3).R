cat('Begin Running file : ', "Loading_the_data_file.R", " ...", '\n')
cat('--------------------------------------------------------', '\n')
file.time <- Sys.time()
cat('Clearing up workspace.', '\n')
cat('----------------------', '\n')
{
  rm(list = ls())
  source(file = "00_00_Preamble.R")
  packages<-c("ggplot2", "foreach", "doParallel", "tidyr", "dplyr", "caret", "stringr", "gmm", "magrittr", "quanteda", "tidytext", "tm", "SnowballC")
  # remove.packages(packages, lib = .libPaths())
  check.packages(packages)
}


cat('Load the reviews data.', '\n')
cat('----------------------', '\n')
{
  load(file = paste(project.path,"Output/TEMP/","11_00_Reviews_data_plain.RData", sep=""))
  review.plain.data <- review.plain.data %>% mutate(rev_date = date) 
  review.plain.data$rev_date <- as.Date(review.plain.data$rev_date, "%d%b%Y")
  review.plain.data <- review.plain.data %>% arrange(propertyid, rev_date) 
}

review_corpus = Corpus(VectorSource(review.plain.data$comments))
review_corpus = tm_map(review_corpus, content_transformer(tolower))
review_corpus = tm_map(review_corpus, removeNumbers)
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus = tm_map(review_corpus, removeWords, c("the", "and", stopwords("english")))
review_corpus =  tm_map(review_corpus, stripWhitespace)


inspect(review_corpus[1])


review_dtm <- DocumentTermMatrix(review_corpus)
review_dtm



inspect(review_dtm[500:505, 500:505])


review_dtm = removeSparseTerms(review_dtm, 0.99)
review_dtm


inspect(review_dtm[1,1:20])

findFreqTerms(review_dtm, 1000)


freq = data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))

review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))
review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.99)
review_dtm_tfidf

# The first document
inspect(review_dtm_tfidf[1,1:20])


freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))


review.plain.data$comments = NULL

review.plain.data = cbind(review.plain.data, as.matrix(review_dtm_tfidf))
object.size(review.plain.data)/1000000000











# =====================================================================================================================
# stopwords.list <- c(stopwords('en'),stopwords('fr'), stopwords('es'), stopwords('pt'), stopwords('de'), stopwords(language = "zh", source = "misc"), stopwords('ru'))
# stopwords.list <- c(stopwords('en'))
language.list <- setdiff(getStemLanguages(),c("porter", "turkish"))
stopwords.list <- unlist(c(lapply(X = language.list, FUN = stopwords)), recursive = TRUE, use.names = TRUE)
  quanteda_options(threads = detectCores() - 1)
quanteda_options(language_stemmer = 'English')
# =====================================================================================================================

# Loading the text data
# ---------------------------------------------------------------------------------------------------------------------
start.loadReview <- Sys.time()
load(file = paste(project.path,"Output/TEMP/","11_00_Reviews_data_plain.RData", sep=""))
review.plain.data <- review.plain.data %>% mutate(rev_date = date) 
review.plain.data$rev_date <- as.Date(review.plain.data$rev_date, "%d%b%Y")
review.plain.data <- review.plain.data %>% arrange(propertyid, rev_date) 
end.loadReview <- Sys.time()
cat('Time take to load reviews data is : ')
print(end.loadReview - start.loadReview)
gc()

# Tokenizing and preprocessing
# ---------------------------------------------------------------------------------------------------------------------

# *********************************************************************************************************************
# Ngrams also
review.tokens <- tokens(review.plain.data$comments, 
                        what = "word", 
                        remove_numbers = TRUE, 
                        remove_punct = TRUE,
                        remove_symbols = TRUE, 
                        remove_hyphens = TRUE, 
                        remove_twitter = TRUE, 
                        remove_url = TRUE, 
                        remove_separators = TRUE
)                             # ngrams = 1:2, skip = 0:1, concatenator = "_"
# *********************************************************************************************************************

# Converting each token to lowe case
review.tokens <- tokens_tolower(review.tokens)
# Removing stopwords used in multiple languages, not just english
review.tokens <- tokens_select(review.tokens, pattern = stopwords.list, selection = "remove")
# Stemming the tokens to make run running the same word
review.tokens <- tokens_wordstem(review.tokens, language = getStemLanguages())

# Converting to a document feature matrix
# ---------------------------------------------------------------------------------------------------------------------
review.tokens.dfm <- dfm(review.tokens, tolower = FALSE)
# head(docnames(review.tokens.dfm), 20)
# ndoc(review.tokens.dfm)
# nfeat(review.tokens.dfm)

# Begin the process of converting to a usable data.
# ---------------------------------------------------------------------------------------------------------------------
# The DFM contains a lot of features which is difficult to convert to a data frame. 
# Most of these features are sparse. We try to remove the most sparse ones from the DFM.
# We realize that with the current capacity, we can only covert about 1800 features to a DF.
# Anyway most of them are about useless since they are so sprse. 
# Use functionality of trim and ngrams to test between various specifications of this text data.

# *********************************************************************************************************************
# Trim the DFM
text.df <- dfm_trim(review.tokens.dfm, min_termfreq = 2500, termfreq_type = "count", verbose = TRUE)     
# min_termfreq = NULL, max_termfreq = NULL,
# termfreq_type = c("count", "prop", "rank", "quantile"),
# min_docfreq = NULL, max_docfreq = NULL, docfreq_type = c("count",
# "prop", "rank", "quantile")
print(text.df)
# *********************************************************************************************************************

# Compute TF-IDF matrix
text.df <-  dfm_tfidf(text.df, scheme_tf = "count", scheme_df = "inverse", base = 10, force = FALSE)

# Convert to a data frame 
text.df <- convert(text.df, to = "data.frame")
gc()

# Combine with original to keep the rental and date information.
text.df <- bind_cols(review.plain.data, text.df, .id = NULL) 

# *** UNCOMMENT IN THE FINAL CODE RUN ***
# Remove unnecessary objects
rm(list = c("end.loadReview", 
            "language.list",
            "review.plain.data", 
            "review.tokens", 
            "review.tokens.dfm", 
            "start.loadReview",
            "stopwords.list" ))

gc()

# Collect reviews that are posted on same day
text.df <- text.df %>%
  select(-reviewer_id, -id,-reviewer_name,-comments,-document) %>% 
  group_by(propertyid , rev_date, date) %>% 
  summarize_all(list(textvar = sum),  na.rm = TRUE) %>% as.tbl()

# Filling missing values with zeros.
text.df[is.na(text.df)] <-0
print(text.df)

# Cumulating the data and save.
text.df <- text.df %>% arrange(propertyid, rev_date) %>% group_by(propertyid) %>% mutate_at(vars(-propertyid,-rev_date,-date),cumsum)
text.df <- text.df %>% select(-rev_date) %>% rename(rev_date=date) 
object.size(text.df) %>% print()
save.time.begin <- Sys.time()
save(text.df, file = paste(project.path, "Output/TEMP/", "22_01_text_df.RData", sep = ""))
save.time.end <- Sys.time()
cat('Time taken to save text df object : ', '\n')
print(save.time.end-save.time.begin)
gc()
print(text.df)
# =====================================================================================================================

# # ***DON'T COMMENT IF RUNNING FRESH***
# # ***THIS MAY BE JUNK NOW***
# # Creating a rental time panel
# # ---------------------------------------------------------------------------------------------------------------------
# ptm.PanelPrep <- proc.time()
# 
# # Use rev_date to match review data with the panel.
# 
# # *********************************************************************************************************************
# # How many reviews were accumulated before booking.
# demand.data$date = as.Date(demand.data$date,format="%d%b%Y")
# rental.time.panel <-  demand.data %>%             # mutate(rev_date = either booking date or date of stay)
#   select(propertyid, date, rev_date) %>%
#   as.tbl()
# # *********************************************************************************************************************
# 
# print(rental.time.panel)
# save(rental.time.panel, file = paste(project.path, "Output/TEMP/", "21_01_rental_time_panel.RData", sep = ""))
# rm(rental.time.panel)
# gc()
# cat('Time taken to prepare the panel : ')
# print(proc.time() - ptm.PanelPrep)
# # =====================================================================================================================
# # =====================================================================================================================


# text.df <- text.df %>% 
#   full_join(rental.time.panel, by = c("propertyid", "rev_date")) 
#   
# text.df[is.na(text.df)] <-0


# # DF <- tidy(review.tokens.dfm)
# # DF2 <- DF %>% cast_dtm(document, term, count)
# # convert(review.tokens.dfm, to = c("matrix"), docvars = NULL)
# # review.tokens.df <- as.data.frame(review.tokens.dfm)
# # colnames(DF2)
# 
# review.tokens.dfm
# 
# dfm.trim <- dfm_trim(review.tokens.dfm, max_docfreq = 1974)
# print(dfm.trim)
# textplot_wordcloud(dfm.trim, color = rev(RColorBrewer::brewer.pal(10, "RdBu")))
# 
# 
# dfm.trim <- dfm_trim(review.tokens.dfm, min_docfreq = 1975)
# print(dfm.trim)
# textplot_wordcloud(dfm.trim, color = rev(RColorBrewer::brewer.pal(10, "RdBu")))
# 
# 
# dfm.trim <- dfm_trim(review.tokens.dfm, max_termfreq = 1974)
# print(dfm.trim)
# textplot_wordcloud(dfm.trim, color = rev(RColorBrewer::brewer.pal(10, "RdBu")))
# 
# 
# dfm.trim <- dfm_trim(review.tokens.dfm, min_termfreq = 2000)
# print(dfm.trim)
# textplot_wordcloud(dfm.trim, color = rev(RColorBrewer::brewer.pal(10, "RdBu")))
# 
# 
# 
# DF4 <- convert(dfm.trim, to = "data.frame")
# 
# 
# 
# 
# 
# 
# # review.panel <- rental.time.panel %>% left_join(review.data, by = c("propertyid", "date"))
# # rm(rental.time.panel)
# # rm(review.data)
# 
# 
# # # Take a sample (this part need to be commented out)
# # # ---------------------------------------------------------------------------------------------------------------------
# # # demand.data.2 <- demand.data
# # set.seed(48374)
# # review.plain.data <- sample_frac(review.plain.data, size = 0.02)
# # # =====================================================================================================================



