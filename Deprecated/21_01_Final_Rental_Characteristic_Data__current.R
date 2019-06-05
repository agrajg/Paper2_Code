library(dplyr)
library(magrittr)
library(data.table)

rental.data.bed_ameneties <- rental.data %>% select(propertyid, bed_amenities) %>% setDT()
rental.data.bed_ameneties <- rental.data.bed_ameneties[order(propertyid)]

{
  cat('* Tokenizing and preprocessing', '\n')
  cat('* ----------------------------', '\n')
  {
    time <- Sys.time()
    cat('* Creating tokens Unigram bigram and bigram with skip ...', '\n')
    bed_amenities.tokens <- tokens(rental.data.bed_ameneties$bed_amenities, 
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
    bed_amenities.tokens <- tokens_tolower(bed_amenities.tokens)
    # Removing stopwords used in multiple languages, not just english
    bed_amenities.tokens <- tokens_select(bed_amenities.tokens, pattern = stopwords.list, selection = "remove")
    # Stemming the tokens to make run running the same word
    bed_amenities.tokens <- tokens_wordstem(bed_amenities.tokens, language = getStemLanguages())
    bed_amenities.tokens <- tokens_ngrams(bed_amenities.tokens, n = c(1,2), skip = c(0,1), concatenator = "_")
    cat('* Tokenizing and preprocessing', '\n')
    cat('* Time taken : ', '\n')
    print(Sys.time() - time)
    cat('* ============================', '\n')
  }
  
  cat('* Converting to a document feature matrix', '\n')
  cat('* ---------------------------------------', '\n')
  {
    time <- Sys.time()
    bed_amenities.tokens.dfm <- dfm(bed_amenities.tokens, tolower = FALSE)
    cat('* Initial DFM : ', '\n')
    print(bed_amenities.tokens.dfm)
    cat('* Removing bed_amenities.tokens ... ', '\n')
    rm(bed_amenities.tokens)
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
    bed_amenities.tokens.dfm_trim <- dfm_trim(bed_amenities.tokens.dfm, docfreq_type = "prop",  min_docfreq  = 0.000001)
    cat('* DFM after trim (min_docfreq  =', 0.000001,') : ', '\n')
    print(bed_amenities.tokens.dfm_trim)
    cat('* Removing bed_amenities.tokens.dfm ... ', '\n')
    rm(bed_amenities.tokens.dfm)
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
    bed_amenities.tokens.dfm_trim.tfidf <- dfm_tfidf(bed_amenities.tokens.dfm_trim, scheme_tf = "count", scheme_df = "inverse", base = 10, force = FALSE)
    cat('* DFM after TF_IDF conversion (min_docfreq  =', min_docfreq.prop,') : ', '\n')
    print(bed_amenities.tokens.dfm_trim.tfidf)
    cat('* *** NOTE ***', '\n')
    cat('* This TF-IDF Matrix is still too large to convert to a matrix/data frame.', '\n')
    cat('* So I break them in small objects with 200 features.', '\n')
    cat('* ************', '\n')
    cat('* Removing bed_amenities.tokens.dfm_trim ... ', '\n')
    rm(bed_amenities.tokens.dfm_trim)
    gc()
    cat('* Compute TF-IDF matrix.','\n')
    cat('* Time taken : ', '\n')
    print(Sys.time() - time)
    cat('* ======================','\n')
  }
  
  cat('* Converting small matrix into rental time panel', '\n')
  cat('* --------------------------------------------------', '\n')
  {
    cat('* Trim again (200 features) after TF-IDF to make convertible matrix', '\n')
    
    cat('* Begin for loop.', '\n')
    cat('* ---------------', '\n')
    for(i in seq(from = 1, to = nfeat(bed_amenities.tokens.dfm_trim.tfidf), by = 200)){
      # i=1
      cat('* For loop begins ... ', '\n')
      cat('* Features from ', i , ' to ', (i+199), ' ... ' ,'\n')
      
      {
        cat('* Trimming ... ', '\n')
        tfidf_trim200 <- dfm_trim(bed_amenities.tokens.dfm_trim.tfidf, docfreq_type = "rank", max_docfreq = i , min_docfreq = (i+199))
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
        names(text.DT) <- paste(colnames(text.DT), "bed_am", sep="_")
        cat('* Removing tfidf_trim200 ... ', '\n')
        rm(tfidf_trim200)
        gc()
        cat('* Convert to a data table.','\n')
        cat('* Time taken : ', '\n')
        print(Sys.time() - time)
        cat('* ========================','\n')
      }
      
      cat('* Combine with original rental level data.','\n')
      cat('* --------------------------------------------------------------','\n')
      {
        time <- Sys.time()
        cat('* Combining text data table to original review data table ... ', '\n')
        text.DT <- cbind(rental.data.bed_ameneties, text.DT)
        cat('* Remove columns : bed_amenities ... ', '\n')
        text.DT <- text.DT[ , -c("bed_amenities")]
        cat('* Combine with original to keep the rental and date information.','\n')
        cat('* Time taken : ', '\n')
        print(Sys.time() - time)
        cat('* ==============================================================','\n')
      }
      
      cat('* Final rental level ameneties data and save.', '\n')
      cat('* -------------------------------', '\n')
      {
        time <- Sys.time()
        cat('* Ordering ... ', '\n')
        text.DT <- text.DT[order(propertyid)]
        
        cat('* Removing propertyid  ... ', '\n')
        text.DT <- text.DT[ , !c("propertyid", "document")]
        cat('* Writing the csv ... ', '\n')
        fwrite(x = text.DT, 
               file = paste(project.path, "Output/TEMP/ForH2O_GLRM/", "21_01_bed_am_DT_", as.character(i), ".csv", sep = ""), 
               append = FALSE)
        rm(text.DT)
        gc()
        cat('* Final rental level ameneties data and save.', '\n')
        cat('* Time taken : ', '\n')
        print(Sys.time() - time)
        cat('* ===============================', '\n')
      }
    }
    cat('* End for loop.', '\n')
    cat('* =============', '\n')
    rm(bed_amenities.tokens.dfm_trim.tfidf)
    gc()
  }
}

rental.data.desc <- rental.data %>% select(propertyid, desc) %>% setDT()
rental.data.desc <- rental.data.desc[order(propertyid)]

{
  cat('* Tokenizing and preprocessing', '\n')
  cat('* ----------------------------', '\n')
  {
    time <- Sys.time()
    cat('* Creating tokens Unigram bigram and bigram with skip ...', '\n')
    desc.tokens <- tokens(rental.data.desc$desc, 
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
    desc.tokens <- tokens_tolower(desc.tokens)
    # Removing stopwords used in multiple languages, not just english
    desc.tokens <- tokens_select(desc.tokens, pattern = stopwords.list, selection = "remove")
    # Stemming the tokens to make run running the same word
    desc.tokens <- tokens_wordstem(desc.tokens, language = getStemLanguages())
    desc.tokens <- tokens_ngrams(desc.tokens, n = c(1,2), skip = c(0,1), concatenator = "_")
    cat('* Tokenizing and preprocessing', '\n')
    cat('* Time taken : ', '\n')
    print(Sys.time() - time)
    cat('* ============================', '\n')
  }
  
  cat('* Converting to a document feature matrix', '\n')
  cat('* ---------------------------------------', '\n')
  {
    time <- Sys.time()
    desc.tokens.dfm <- dfm(desc.tokens, tolower = FALSE)
    cat('* Initial DFM : ', '\n')
    print(desc.tokens.dfm)
    cat('* Removing desc.tokens ... ', '\n')
    rm(desc.tokens)
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
    desc.tokens.dfm_trim <- dfm_trim(desc.tokens.dfm, docfreq_type = "prop",  min_docfreq  = min_docfreq.prop)
    cat('* DFM after trim (min_docfreq  =', min_docfreq.prop,') : ', '\n')
    print(desc.tokens.dfm_trim)
    cat('* Removing desc.tokens.dfm ... ', '\n')
    rm(desc.tokens.dfm)
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
    desc.tokens.dfm_trim.tfidf <- dfm_tfidf(desc.tokens.dfm_trim, scheme_tf = "count", scheme_df = "inverse", base = 10, force = FALSE)
    cat('* DFM after TF_IDF conversion (min_docfreq  =', min_docfreq.prop,') : ', '\n')
    print(desc.tokens.dfm_trim.tfidf)
    cat('* *** NOTE ***', '\n')
    cat('* This TF-IDF Matrix is still too large to convert to a matrix/data frame.', '\n')
    cat('* So I break them in small objects with 200 features.', '\n')
    cat('* ************', '\n')
    cat('* Removing desc.tokens.dfm_trim ... ', '\n')
    rm(desc.tokens.dfm_trim)
    gc()
    cat('* Compute TF-IDF matrix.','\n')
    cat('* Time taken : ', '\n')
    print(Sys.time() - time)
    cat('* ======================','\n')
  }
  
  cat('* Converting small matrix into rental time panel', '\n')
  cat('* --------------------------------------------------', '\n')
  {
    cat('* Trim again (200 features) after TF-IDF to make convertible matrix', '\n')
    
    cat('* Begin for loop.', '\n')
    cat('* ---------------', '\n')
    for(i in seq(from = 1, to = nfeat(desc.tokens.dfm_trim.tfidf), by = 200)){
      # i=1
      cat('* For loop begins ... ', '\n')
      cat('* Features from ', i , ' to ', (i+199), ' ... ' ,'\n')
      
      {
        cat('* Trimming ... ', '\n')
        tfidf_trim200 <- dfm_trim(desc.tokens.dfm_trim.tfidf, docfreq_type = "rank", max_docfreq = i , min_docfreq = (i+199))
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
        names(text.DT) <- paste(colnames(text.DT), "desc", sep="_")
        cat('* Removing tfidf_trim200 ... ', '\n')
        rm(tfidf_trim200)
        gc()
        cat('* Convert to a data table.','\n')
        cat('* Time taken : ', '\n')
        print(Sys.time() - time)
        cat('* ========================','\n')
      }
      
      cat('* Combine with original rental level data.','\n')
      cat('* --------------------------------------------------------------','\n')
      {
        time <- Sys.time()
        cat('* Combining text data table to original review data table ... ', '\n')
        text.DT <- cbind(rental.data.desc, text.DT)
        cat('* Remove columns : desc ... ', '\n')
        text.DT <- text.DT[ , -c("desc")]
        cat('* Combine with original rental level data.','\n')
        cat('* Time taken : ', '\n')
        print(Sys.time() - time)
        cat('* ==============================================================','\n')
      }
      
      cat('* Final rental level desc data and save.', '\n')
      cat('* -------------------------------', '\n')
      {
        time <- Sys.time()
        cat('* Ordering ... ', '\n')
        text.DT <- text.DT[order(propertyid)]
        
        cat('* Removing propertyid  ... ', '\n')
        text.DT <- text.DT[ , !c("propertyid", "document")]
        cat('* Writing the csv ... ', '\n')
        fwrite(x = text.DT, 
               file = paste(project.path, "Output/TEMP/ForH2O_GLRM/", "21_01_desc_DT_", as.character(i), ".csv", sep = ""), 
               append = FALSE)
        rm(text.DT)
        gc()
        cat('* Final rental level desc data and save.', '\n')
        cat('* Time taken : ', '\n')
        print(Sys.time() - time)
        cat('* ===============================', '\n')
      }
    }
    cat('* End for loop.', '\n')
    cat('* =============', '\n')
    rm(desc.tokens.dfm_trim.tfidf)
    gc()
  }
}


rental.data.allchar <- rental.data %>% select(-bed_amenities, -desc) %>% setDT()
rental.data.allchar <- rental.data.allchar[order(propertyid)]

cat('* Final rental level other char data and save.', '\n')
cat('* -------------------------------', '\n')
{
  time <- Sys.time()
  cat('* Ordering ... ', '\n')
  pid.data <- pid.data %>% setDT()
  rental.data.allchar <- pid.data[rental.data.allchar, on="propertyid"]                               # left join
  
  rental.data.allchar <- rental.data.allchar[order(propertyid)]
  
  cat('* Removing propertyid  ... ', '\n')
  rental.data.allchar <- rental.data.allchar[ , !c("propertyid")]
  cat('* Writing the csv ... ', '\n')
  fwrite(x = rental.data.allchar, 
         file = paste(project.path, "Output/TEMP/ForH2O_GLRM/", "21_01_all_char_DT", ".csv", sep = ""), 
         append = FALSE)
  rm(rental.data.allchar)
  gc()
  cat('* Final rental level other char data and save.', '\n')
  cat('* Time taken : ', '\n')
  print(Sys.time() - time)
  cat('* ===============================', '\n')
}

# pid.data.temp <- pid.data %>% setDT()
# pid.data.temp <- pid.data.temp[order(propertyid)]
# 
# cat('* Final PID data level other char data and save.', '\n')
# cat('* -------------------------------', '\n')
# {
#   time <- Sys.time()
#   cat('* Ordering ... ', '\n')
#   pid.data.temp <- pid.data.temp[order(propertyid)]
#   
#   cat('* Removing propertyid  ... ', '\n')
#   pid.data.temp <- pid.data.temp[ , !c("propertyid")]
#   cat('* Writing the csv ... ', '\n')
#   fwrite(x = pid.data.temp, 
#          file = paste(project.path, "Output/TEMP/ForH2O_GLRM/", "21_01_PID_DT", ".csv", sep = ""), 
#          append = FALSE)
#   rm(pid.data.temp)
#   gc()
#   cat('* Final rental level other char data and save.', '\n')
#   cat('* Time taken : ', '\n')
#   print(Sys.time() - time)
#   cat('* ===============================', '\n')
# }

