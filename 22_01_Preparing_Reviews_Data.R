library(SnowballC)
cat('* Read Inputs ... ', '\n')
{
  min.doc.percent <- 0.1
  # stopwords.list <- c(stopwords('en'),stopwords('fr'), stopwords('es'), stopwords('pt'), stopwords('de'), stopwords(language = "zh", source = "misc"), stopwords('ru'))
  # stopwords.list <- c(stopwords('en'))
  language.list <- setdiff(getStemLanguages(),c("porter", "turkish"))
  stopwords.list <- unlist(c(lapply(X = language.list, FUN = stopwords)), recursive = TRUE, use.names = TRUE)
}

cat('* Tokenizing and preprocessing', '\n')
cat('* ----------------------------', '\n')
{
  time <- Sys.time()
  cat('* Creating tokens Unigram bigram and bigram with skip ...', '\n')
  review.tokens <- tokens(review.plain.data$comments, 
                          what = "word", 
                          remove_numbers = FALSE, 
                          remove_punct = TRUE,
                          remove_symbols = TRUE, 
                          remove_hyphens = TRUE, 
                          remove_twitter = TRUE, 
                          remove_url = TRUE, 
                          remove_separators = TRUE
  )
  # Converting each token to lowe case
  review.tokens <- tokens_tolower(review.tokens)
  # Removing stopwords used in multiple languages, not just english
  review.tokens <- tokens_select(review.tokens, pattern = stopwords.list, selection = "remove")
  # Stemming the tokens to make run running the same word
  review.tokens <- tokens_wordstem(review.tokens, language = getStemLanguages())
  review.tokens <- tokens_ngrams(review.tokens, n = c(1,2), skip = c(0,1), concatenator = "_")
  cat('* Tokenizing and preprocessing', '\n')
  cat('* Time taken : ', '\n')
  print(Sys.time() - time)
  cat('* ============================', '\n')
}

cat('* Converting to a document feature matrix', '\n')
cat('* ---------------------------------------', '\n')
{
  time <- Sys.time()
  review.tokens.dfm <- dfm(review.tokens, tolower = FALSE)
  cat('* Initial DFM : ', '\n')
  print(review.tokens.dfm)
  cat('* Removing review.tokens ... ', '\n')
  rm(review.tokens)
  gc()
  cat('* Converting to a document feature matrix', '\n')
  cat('* Time taken : ', '\n')
  print(Sys.time() - time)
  cat('* =======================================', '\n')
}

cat('* Perform trimming.', '\n')
cat('* -----------------', '\n')
{
  time <- Sys.time()
  review.tokens.dfm_trim <- dfm_trim(review.tokens.dfm, docfreq_type = "prop",  min_docfreq  = (min.doc.percent/100))
  cat('* DFM after trim (min_docfreq  =', (min.doc.percent/100) ,') : ', '\n')
  print(review.tokens.dfm_trim)
  cat('* Removing review.tokens.dfm ... ', '\n')
  rm(review.tokens.dfm)
  gc()
  cat('* Perform trimming.', '\n')
  cat('* Time taken : ', '\n')
  print(Sys.time() - time)
  cat('* =================', '\n')
}

cat('* Compute TF-IDF matrix.','\n')
cat('* ----------------------','\n')
{
  time <- Sys.time()
  review.tokens.dfm_trim.tfidf <- dfm_tfidf(review.tokens.dfm_trim, scheme_tf = "count", scheme_df = "inverse", base = 10, force = FALSE)
  cat('* DFM after TF_IDF conversion (min_docfreq  =', (min.doc.percent/100),') : ', '\n')
  print(review.tokens.dfm_trim.tfidf)
  cat('* *** NOTE ***', '\n')
  cat('* This TF-IDF Matrix is still too large to convert to a matrix/data frame.', '\n')
  cat('* So I break them in small objects with 200 features.', '\n')
  cat('* ************', '\n')
  cat('* Removing review.tokens.dfm_trim ... ', '\n')
  rm(review.tokens.dfm_trim)
  gc()
  cat('* Compute TF-IDF matrix.','\n')
  cat('* Time taken : ', '\n')
  print(Sys.time() - time)
  cat('* ======================','\n')
}

cat('* TO CSV : Reviews data without text reviews ... ', '\n')
{
  cat('* Print the dimension of the CSV data ... ', '\n')
  print(dim(review.plain.data[ , !c("reviewer_name", "comments")]))
  cat('* Writing the csv ... ', '\n')
  fwrite(x = review.plain.data[ , !c("reviewer_name", "comments")], 
         file = paste(project.path, "Output/TEMP/ForH2O_Reviews_GLRM/", "22_01_Reviews_Panel", ".csv", sep = ""), 
         append = FALSE)
}

cat('* Converting small matrix into rental time panel', '\n')
cat('* --------------------------------------------------', '\n')
{
  cat('* Trim again (200 features) after TF-IDF to make convertible matrix', '\n')
  
  cat('* Begin for loop.', '\n')
  cat('* ---------------', '\n')
  for(i in seq(from = 1, to = nfeat(review.tokens.dfm_trim.tfidf), by = 200)){
    # i = 1
    cat('* For loop begins ... ', '\n')
    cat('* Features from ', i , ' to ', (i+199), ' ... ' ,'\n')
    
    {
      cat('* Trimming ... ', '\n')
      tfidf_trim200 <- dfm_trim(review.tokens.dfm_trim.tfidf, docfreq_type = "rank", max_docfreq = i , min_docfreq = (i+199))
      cat('* DFM TF IDF object after trim : ', '\n')
      print(tfidf_trim200)
    }
    
    if (nfeat(tfidf_trim200) == 0) {
      cat('* Breaking the loop at i = ', i,'\n')
      rm(tfidf_trim200)
      gc()
      break
    }
    
    cat('* Convert to a data table.', '\n')
    cat('* ------------------------', '\n')
    {
      time <- Sys.time()
      cat('* Converting to df ... ', '\n')
      text.DT <- convert(tfidf_trim200, to = "data.frame") %>% setDT()
      cat('* Renaming all text variables to end with _text ... ', '\n')
      names(text.DT) <- paste(colnames(text.DT), "text", sep="_")
      cat('* Removing tfidf_trim200 ... ', '\n')
      rm(tfidf_trim200)
      gc()
      cat('* Convert to a data table.','\n')
      cat('* Time taken : ', '\n')
      print(Sys.time() - time)
      cat('* ========================','\n')
    }
    
    cat('* TO CSV : TF-IDF 200 batch ... ', '\n')
    cat('* -------------------------------', '\n')
    {
      time <- Sys.time()
      cat('* Print the dimension of the CSV data ... ', '\n')
      print(dim(text.DT[ , !c("document_text")]))
      cat('* Writing the csv ... ', '\n')
      fwrite(x = text.DT[ , !c("document_text")], 
             file = paste(project.path, "Output/TEMP/ForH2O_Reviews_GLRM/", "22_01_Reviews_tfidf_200_data_", as.character(i), ".csv", sep = ""), 
             append = FALSE)
      rm(text.DT)
      gc()
      cat('* Time taken : ', '\n')
      print(Sys.time() - time)
      cat('* ===============================', '\n')
    }
  }
  cat('* End for loop.', '\n')
  cat('* =============', '\n')
  rm(review.tokens.dfm_trim.tfidf)
  gc()
}  

