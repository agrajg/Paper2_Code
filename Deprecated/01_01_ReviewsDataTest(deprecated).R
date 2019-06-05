source('00_00_Preamble.R', echo=FALSE)
load(file = "00_00_Reviews_data.RData")
review.data <- as.data.frame(review.data)

review.data$CorpusLength <- nchar(review.data$corpus)
summary(review.data$CorpusLength)
# head(review.data)


# Sample the data
library(magrittr)
library(dplyr)
# review.data <- review.data %>% filter(CorpusLength < 100)
# review.data <- review.data.temp[sample(1:nrow(review.data), 50, replace=FALSE)]



# Feature Engineering 
# Size of the text. Number of reviews length per review. indicates how old rental
# Text analytics
library(quanteda)
quanteda_options(verbose = TRUE, threads = 60)

# Tokenize reviews doc.
review.data.token <- tokens(review.data$corpus, what = "word", 
                       remove_numbers = FALSE, 
                       remove_punct = TRUE,
                       remove_symbols = TRUE, 
                       remove_hyphens = TRUE, 
                       remove_separators =TRUE, 
                       remove_url = TRUE, 
                       ngrams = 1:1, 
                       skip = 1:1)

review.data.token[[3]]
review.data.token <- tokens_tolower(review.data.token)
review.data.token[[3]]
# review.data.token <- tokens_ngrams(review.data.token, n = 1:2)
# review.data.token[[3]]
# stopwords()
review.data.token <- tokens_select(review.data.token, stopwords(),selection = "remove")
review.data.token[[3]]
review.data.token <- tokens_wordstem(review.data.token, language = "english")
review.data.token[[3]]
# Document Frequency Matrix
review.data.token.dfm <- dfm(review.data.token, tolower = FALSE)
dim(review.data.token.dfm)


# Transform to a matrix and inspect.
review.data.token.matrix <- as.matrix(review.data.token.dfm)
View(review.data.token.matrix[1:20, 1:100])
dim(review.data.token.matrix)
# Investigate the effects of stemming.
colnames(review.data.token.matrix)[1:50]



