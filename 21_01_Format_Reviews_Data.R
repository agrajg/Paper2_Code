# NOTES : Control the number of features on line 31-34.


cat('* Begin Running file : ', '03_01_Format_Reviews_Data.R', " ...", '\n')
cat('* ------------------------------------------------------------', '\n')
{
  cat('* R setup', '\n')
  cat('* -------', '\n')
  {
    rm(list = ls())
    source(file = "00_00_Preamble.R")
    # Packages needed in this program
    packages<-c("slam", "Matrix","ggplot2", "foreach", "doParallel", "tidyr", "dplyr", "data.table", "stringr", 
                "gmm", "magrittr", "quanteda", "tidytext", "tm", "SnowballC", "devtools", "rlang")
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
  
  source(file = "11_01_Loading_Demand_Review_Data.R")
  source(file = "12_01_Create_Sample.R")
  
  cat('* Creating a bench mark panel sorted by property id and date ... ')
  {
    text.panel.DT.original <-  demand.data %>% 
      select(propertyid, date) %>% setDT()
    text.panel.DT.original <- text.panel.DT.original[order(propertyid, date)]
  }
  
  
  cat('* Setting up a check for consistancy.', '\n')
  cat('* -----------------------------------', '\n')
  {
    cat('* Randomly pick 5 observation numbers')
    check_rows = sample(1:dim(text.panel.DT.original)[1] , size = 5)
    cat('* Check consistancy of text.panel.DT.original ... ', '\n')
    cat('* Checking rows ... ', '\n')
    print(check_rows)
    cat('* Check propertyid ... ', '\n')
    print(text.panel.DT.original$propertyid[check_rows])
    cat('* Check date ... ', '\n')
    print(text.panel.DT.original$date[check_rows])
    cat('* Check consistancy of demand.data ... ', '\n')
    cat('* Check propertyid ... ', '\n')
    print(demand.data$propertyid[check_rows])
    cat('* Check date ... ', '\n')
    print(demand.data$date[check_rows])
    cat('* Setting up a check for consistancy.', '\n')
    cat('* ===================================', '\n')
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
    review.tokens.dfm_trim <- dfm_trim(review.tokens.dfm, docfreq_type = "prop",  min_docfreq  = min_docfreq.prop)
    cat('* DFM after trim (min_docfreq  =', min_docfreq.prop,') : ', '\n')
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
    cat('* DFM after TF_IDF conversion (min_docfreq  =', min_docfreq.prop,') : ', '\n')
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
  
  cat('* Converting small matrix into rental time panel', '\n')
  cat('* --------------------------------------------------', '\n')
  {
    cat('* Trim again (200 features) after TF-IDF to make convertible matrix', '\n')
    
    cat('* Begin for loop.', '\n')
    cat('* ---------------', '\n')
    for(i in seq(from = 1, to = nfeat(review.tokens.dfm_trim.tfidf), by = 200)){
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
      
      
      cat('* Combine with original to keep the rental and date information.','\n')
      cat('* --------------------------------------------------------------','\n')
      {
        time <- Sys.time()
        cat('* Combining text data table to original review data table ... ', '\n')
        text.DT <- cbind(review.plain.data, text.DT)
        cat('* Remove columns : reviewer_id , id , reviewer_name , comments , document ... ', '\n')
        text.DT <- text.DT[ , -c("reviewer_id", "id","reviewer_name","comments","document_text")]
        cat('* Collect reviews that are posted on same day ... ', '\n')
        text.DT <- text.DT[ , lapply(.SD,sum, na.rm=TRUE),by=c("propertyid", "rev_date")]
        # text.DT <- text.DT %>%
        #   select(-reviewer_id, -id,-reviewer_name,-comments,-document) %>% 
        #   group_by(propertyid , rev_date, date) %>% 
        #   summarize_all(list(textvar = sum),  na.rm = TRUE)
        
        cat('* Combine with original to keep the rental and date information.','\n')
        cat('* Time taken : ', '\n')
        print(Sys.time() - time)
        cat('* ==============================================================','\n')
      }
      
      cat('* Merging with the rental date panel.', '\n')
      cat('* -----------------------------------', '\n')
      {
        time <- Sys.time()
      
        cat('* Creating text panel with variable rev_date to merge on (can be changed based on booking date) ... ')
        text.panel.DT <- text.panel.DT.original %>% 
          select(propertyid, date) %>% 
          mutate(rev_date  = date) %>% setDT()
        cat('* Size of the panel : '  , '\n')
        print(object.size(text.panel.DT), units = "auto", standard = "SI")
        
        cat('* Secluding those that will not merge ... ', '\n')
        text.panel.DT.not_merge <- text.DT[!text.panel.DT, on=c("propertyid", "rev_date")]
        text.panel.DT.not_merge <- text.panel.DT.not_merge %>% mutate(date = rev_date) %>% setDT()
        
        cat('* Merging rental panel with text data to create text panel ... ', '\n')
        text.panel.DT <- text.DT[text.panel.DT, on=c("propertyid", "rev_date")]
        
        cat('* Rbind additional rows that were not merged ... ' , '\n')
        text.panel.DT <- rbind(text.panel.DT, text.panel.DT.not_merge)
        
        cat('* Size of the panel + merged text data: '  , '\n')
        print(object.size(text.panel.DT), units = "auto", standard = "SI")
        
        cat('* Removing text.DT ... ', '\n')
        rm(text.DT)
        gc()
        
        cat('* Removing text.panel.DT.not_merge ... ', '\n')
        rm(text.panel.DT.not_merge)
        gc()
        
        cat('* Merging with the rental date panel.', '\n')
        cat('* Time taken : ', '\n')
        print(Sys.time() - time)
        cat('* =============================', '\n')
      }
      
      cat('* Replacing missing with 0', '\n')
      cat('* ------------------------', '\n')
      {
        time <- Sys.time()
        text.panel.DT[is.na(text.panel.DT)] <- 0
        
        cat('* Size of the panel + merged text data + zeros: '  , '\n')
        print(object.size(text.panel.DT), units = "auto", standard = "SI")
        
        cat('* Replacing missing with 0', '\n')
        cat('* Time taken : ', '\n')
        print(Sys.time() - time)
        cat('* ========================', '\n')
      }
      
      cat('* Sorting by propertyid and date for cumsum.' ,'\n')
      cat('* ------------------------------------------' ,'\n')
      {
        time <- Sys.time()
        text.panel.DT <- text.panel.DT[order(propertyid,date)]
        
        cat('* Sorting by propertyid and date for cumsum.' ,'\n')
        cat('* Time taken : ', '\n')
        print(Sys.time() - time)
        cat('* ==========================================' ,'\n')
      }  
      
      cat('* Calculating the cumulative sum.', '\n')
      cat('* -------------------------------', '\n')
      {
        time <- Sys.time()
        text.panel.DT <- text.panel.DT %>% group_by(propertyid) %>% mutate_at(vars(-propertyid,-rev_date,-date),cumsum)
        cat('* No need of rev_date anymore ... ', '\n')
        text.panel.DT <- text.panel.DT %>% select(-rev_date) %>% setDT()
        
        cat('* Size of the panel + merged text data + zeros + cumulation '  , '\n')
        print(object.size(text.panel.DT), units = "auto", standard = "SI")
        
        cat('* Subset by dates ... ', '\n')
        text.panel.DT <- text.panel.DT[(date >= "2014-09-01" & date <= "2017-03-31")]
        
        cat('* Calculating the cumulative sum.', '\n')
        cat('* Time taken : ', '\n')
        print(Sys.time() - time)
        cat('* ===============================', '\n')
      }
      
      cat('* Merge with Original Text panel.', '\n')
      cat('* -------------------------------', '\n')  
      {
        time <- Sys.time()
        cat('* Left merge with original panel ... ', '\n')
        text.panel.DT <- text.panel.DT[text.panel.DT.original, on=c("propertyid", "date")]
        
        cat('* Size of the final panel data with reviews : '  , '\n')
        print(object.size(text.panel.DT), units = "auto", standard = "SI")
        
        cat('* Merge with Original Text panel.', '\n')
        cat('* Time taken : ', '\n')
        print(Sys.time() - time)
        cat('* ===============================', '\n')  
      }
      
      cat('* Final text panel prep and save.', '\n')
      cat('* -------------------------------', '\n')
      {
        time <- Sys.time()
        cat('* Ordering ... ', '\n')
        text.panel.DT <- text.panel.DT[order(propertyid, date)]
        
        {
          cat('* Check consistancy ... ', '\n')
          cat('* Checking rows ... ', '\n')
          print(check_rows)
          cat('* Check propertyid ... ', '\n')
          print(text.panel.DT$propertyid[check_rows])
          cat('* Check date ... ', '\n')
          print(text.panel.DT$date[check_rows])
        }
        
        cat('* Removing propertyid and date ... ', '\n')
        text.panel.DT <- text.panel.DT[ , !c("propertyid", "date")]
        cat('* Writing the csv ... ', '\n')
        fwrite(x = text.panel.DT, 
               file = paste(project.path, "Output/TEMP/ForH2O/", "21_01_text_panel_DT_", as.character(i), ".csv", sep = ""), 
               append = FALSE)
        rm(text.panel.DT)
        gc()
        cat('* Final text panel prep and save.', '\n')
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
  
  cat('* Final demand panel prep and save.', '\n')
  cat('* ---------------------------------', '\n')
  {
    time <- Sys.time()
    cat('* Ordering ... ', '\n')
    demand.data <- demand.data[order(propertyid, date)]

    {      
      cat('* Check consistancy ... ', '\n')
      cat('* Checking rows ... ', '\n')
      print(check_rows)
      cat('* Check propertyid ... ', '\n')
      print(demand.data$propertyid[check_rows])
      cat('* Check date ... ', '\n')
      print(demand.data$date[check_rows])
    }
    
    cat('* Writing the csv ... ', '\n')
    fwrite(x = demand.data, 
           file = paste(project.path, "Output/TEMP/ForH2O/", "21_01_Demand_panel_DT", ".csv", sep = ""),
           append = FALSE)
    gc()
    cat('* Final demand panel prep and save.', '\n')
    cat('* Time taken : ', '\n')
    print(Sys.time() - time)
    cat('* =================================', '\n')
  }
  cat('* End Running file : ', '03_01_Format_Reviews_Data.R', " ...", '\n')
  cat('* Time taken : ', '\n')
  print(Sys.time() - file.time)
  cat('* =========================XXXXXXXX=========================', '\n')
}


