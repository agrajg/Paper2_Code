library(dplyr)
library(magrittr)
library(data.table)
library(quanteda)
library(SnowballC)

rental.data.allchar <- rental.data %>% select(-bed_amenities, -desc) %>% setDT()
rental.data.allchar <- rental.data.allchar[order(propertyid)]
{
  # rental.data.allchar[text.panel.DT.original, on="propertyid"]                               # left join
  cat('# Merging with the rental date panel.', '\n')
  cat('# -----------------------------------', '\n')
  {
    time <- Sys.time()
    
    cat('# Merging rental panel with text data to create text panel ... ', '\n')
    allchar.panel.DT <- rental.data.allchar[text.panel.DT.original, on=c("propertyid")]
    
    cat('# Size of the panel + merged text data: '  , '\n')
    print(object.size(allchar.panel.DT), units = "auto", standard = "SI")
    
    cat('# Removing text.DT ... ', '\n')
    rm(rental.data.allchar)
    gc()
    
    cat('# Merging with the rental date panel.', '\n')
    cat('# Time taken : ', '\n')
    print(Sys.time() - time)
    cat('# =============================', '\n')
  }
  
  # cat('# Replacing missing with 0', '\n')
  # cat('# ------------------------', '\n')
  # {
  #   time <- Sys.time()
  #   allchar.panel.DT[is.na(allchar.panel.DT)] <- 0
  #   
  #   cat('# Size of the panel + merged text data + zeros: '  , '\n')
  #   print(object.size(allchar.panel.DT), units = "auto", standard = "SI")
  #   
  #   cat('# Replacing missing with 0', '\n')
  #   cat('# Time taken : ', '\n')
  #   print(Sys.time() - time)
  #   cat('# ========================', '\n')
  # }
  
  cat('# Sorting by propertyid and date for cumsum.' ,'\n')
  cat('# ------------------------------------------' ,'\n')
  {
    time <- Sys.time()
    allchar.panel.DT <- allchar.panel.DT[order(propertyid,date)]
    
    cat('# Sorting by propertyid and date for cumsum.' ,'\n')
    cat('# Time taken : ', '\n')
    print(Sys.time() - time)
    cat('# ==========================================' ,'\n')
  }  
  
  cat('# Merge with Original Text panel.', '\n')
  cat('# -------------------------------', '\n')  
  {
    time <- Sys.time()
    cat('# Left merge with original panel ... ', '\n')
    allchar.panel.DT <- allchar.panel.DT[text.panel.DT.original, on=c("propertyid", "date")]
    
    cat('# Size of the final panel data with bed_amenitiess : '  , '\n')
    print(object.size(allchar.panel.DT), units = "auto", standard = "SI")
    
    cat('# Merge with Original Text panel.', '\n')
    cat('# Time taken : ', '\n')
    print(Sys.time() - time)
    cat('# ===============================', '\n')  
  }
  
  cat('# Final text panel prep and save.', '\n')
  cat('# -------------------------------', '\n')
  {
    time <- Sys.time()
    cat('# Ordering ... ', '\n')
    allchar.panel.DT <- allchar.panel.DT[order(propertyid, date)]
    
    {
      cat('# Check consistancy ... ', '\n')
      cat('# Checking rows ... ', '\n')
      print(check_rows)
      cat('# Check propertyid ... ', '\n')
      print(allchar.panel.DT$propertyid[check_rows])
      cat('# Check date ... ', '\n')
      print(allchar.panel.DT$date[check_rows])
    }
    
    cat('# Removing propertyid and date ... ', '\n')
    allchar.panel.DT <- allchar.panel.DT[ , !c("propertyid", "date")]
    cat('# Writing the csv ... ', '\n')
    fwrite(x = allchar.panel.DT, 
           file = paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_allchar_panel_DT", ".csv", sep = ""), 
           append = FALSE)
    rm(allchar.panel.DT)
    gc()
    cat('# Final text panel prep and save.', '\n')
    cat('# Time taken : ', '\n')
    print(Sys.time() - time)
    cat('# ===============================', '\n')
  }
}


rental.data.bed_amenities <- rental.data %>% select(propertyid, bed_amenities) %>% setDT()
rental.data.bed_amenities <- rental.data.bed_amenities[order(propertyid)]

{  
  cat('# Setting up a check for consistancy.', '\n')
  cat('# -----------------------------------', '\n')
  {
    # cat('# Randomly pick 5 observation numbers')
    # check_rows = sample(1:dim(text.panel.DT.original)[1] , size = 5)
    # cat('# Check consistancy of text.panel.DT.original ... ', '\n')
    cat('# Checking rows ... ', '\n')
    print(check_rows)
    cat('# Check propertyid ... ', '\n')
    print(text.panel.DT.original$propertyid[check_rows])
    cat('# Check date ... ', '\n')
    print(text.panel.DT.original$date[check_rows])
    cat('# Check consistancy of demand.data ... ', '\n')
    cat('# Check propertyid ... ', '\n')
    print(demand.data$propertyid[check_rows])
    cat('# Check date ... ', '\n')
    print(demand.data$date[check_rows])
    cat('# Setting up a check for consistancy.', '\n')
    cat('# ===================================', '\n')
  }
  
  cat('# Tokenizing and preprocessing', '\n')
  cat('# ----------------------------', '\n')
  {
    time <- Sys.time()
    cat('# Creating tokens Unigram bigram and bigram with skip ...', '\n')
    bed_amenities.tokens <- tokens(rental.data.bed_amenities$bed_amenities, 
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
    cat('# Tokenizing and preprocessing', '\n')
    cat('# Time taken : ', '\n')
    print(Sys.time() - time)
    cat('# ============================', '\n')
  }
  
  cat('# Converting to a document feature matrix', '\n')
  cat('# ---------------------------------------', '\n')
  {
    time <- Sys.time()
    bed_amenities.tokens.dfm <- dfm(bed_amenities.tokens, tolower = FALSE)
    cat('# Initial DFM : ', '\n')
    print(bed_amenities.tokens.dfm)
    cat('# Removing bed_amenities.tokens ... ', '\n')
    rm(bed_amenities.tokens)
    gc()
    cat('# Converting to a document feature matrix', '\n')
    cat('# Time taken : ', '\n')
    print(Sys.time() - time)
    cat('# =======================================', '\n')
  }
  
  cat('# Perform trimming.', '\n')
  cat('# -----------------', '\n')
  {
    time <- Sys.time()
    bed_amenities.tokens.dfm_trim <- dfm_trim(bed_amenities.tokens.dfm, docfreq_type = "prop",  min_docfreq  = (min.doc.percent/100))
    cat('# DFM after trim (min_docfreq  =', (min.doc.percent/100),') : ', '\n')
    print(bed_amenities.tokens.dfm_trim)
    cat('# Removing bed_amenities.tokens.dfm ... ', '\n')
    rm(bed_amenities.tokens.dfm)
    gc()
    cat('# Perform trimming.', '\n')
    cat('# Time taken : ', '\n')
    print(Sys.time() - time)
    cat('# =================', '\n')
  }
  
  cat('# Compute TF-IDF matrix.','\n')
  cat('# ----------------------','\n')
  {
    time <- Sys.time()
    bed_amenities.tokens.dfm_trim.tfidf <- dfm_tfidf(bed_amenities.tokens.dfm_trim, scheme_tf = "count", scheme_df = "inverse", base = 10, force = FALSE)
    cat('# DFM after TF_IDF conversion (min_docfreq  =', (min.doc.percent/100),') : ', '\n')
    print(bed_amenities.tokens.dfm_trim.tfidf)
    cat('# **# NOTE ***', '\n')
    cat('# This TF-IDF Matrix is still too large to convert to a matrix/data frame.', '\n')
    cat('# So I break them in small objects with 200 features.', '\n')
    cat('# ************', '\n')
    cat('# Removing bed_amenities.tokens.dfm_trim ... ', '\n')
    rm(bed_amenities.tokens.dfm_trim)
    gc()
    cat('# Compute TF-IDF matrix.','\n')
    cat('# Time taken : ', '\n')
    print(Sys.time() - time)
    cat('# ======================','\n')
  }
  
  cat('# Converting small matrix into rental time panel', '\n')
  cat('# --------------------------------------------------', '\n')
  {
    cat('# Trim again (200 features) after TF-IDF to make convertible matrix', '\n')
    
    cat('# Begin for loop.', '\n')
    cat('# ---------------', '\n')
    for(i in seq(from = 1, to = nfeat(bed_amenities.tokens.dfm_trim.tfidf), by = 200)){
      # i=1
      cat('# For loop begins ... ', '\n')
      cat('# Features from ', i , ' to ', (i+199), ' ... ' ,'\n')
      
      {
        cat('# Trimming ... ', '\n')
        tfidf_trim200 <- dfm_trim(bed_amenities.tokens.dfm_trim.tfidf, docfreq_type = "rank", max_docfreq = i , min_docfreq = (i+199))
        cat('# DFM TF IDF object after trim : ', '\n')
        print(tfidf_trim200)
      }
      
      if (nfeat(tfidf_trim200) == 0) {
        cat('# Breaking the loop at i = ', i,'\n')
        rm(tfidf_trim200)
        gc()
        break
      }
      
      cat('# Convert to a data table.', '\n')
      cat('# ------------------------', '\n')
      {
        time <- Sys.time()
        cat('# Converting to df ... ', '\n')
        text.DT <- convert(tfidf_trim200, to = "data.frame") %>% setDT()
        cat('# Renaming all text variables to end with _text ... ', '\n')
        names(text.DT) <- paste(colnames(text.DT), "bed_amenities", sep="_")
        cat('# Removing tfidf_trim200 ... ', '\n')
        rm(tfidf_trim200)
        gc()
        cat('# Convert to a data table.','\n')
        cat('# Time taken : ', '\n')
        print(Sys.time() - time)
        cat('# ========================','\n')
      }
      
      
      cat('# Combine with original to keep the rental information.','\n')
      cat('# --------------------------------------------------------------','\n')
      {
        time <- Sys.time()
        cat('# Combining text data table to original bed_amenities data table ... ', '\n')
        text.DT <- cbind(rental.data.bed_amenities, text.DT)
        cat('# Remove columns : bed_amenitieser_id , id , bed_amenitieser_name , bed_amenities , document ... ', '\n')
        text.DT <- text.DT[ , -c("bed_amenities","document_bed_amenities")]
        # cat('# Collect bed_amenitiess that are posted on same day ... ', '\n')
        # text.DT <- text.DT[ , lapply(.SD,sum, na.rm=TRUE),by=c("propertyid", "rev_date")]
        # text.DT <- text.DT %>%
          #   select(-bed_amenitieser_id, -id,-bed_amenitieser_name,-bed_amenities,-document) %>% 
        #   group_by(propertyid , rev_date, date) %>% 
        #   summarize_all(list(textvar = sum),  na.rm = TRUE)
        
        cat('# Combine with original to keep the rental and date information.','\n')
        cat('# Time taken : ', '\n')
        print(Sys.time() - time)
        cat('# ==============================================================','\n')
      }
      
      cat('# Merging with the rental date panel.', '\n')
      cat('# -----------------------------------', '\n')
      {
        time <- Sys.time()
        
        # cat('# Creating text panel with variable rev_date to merge on (can be changed based on booking date) ... ')
        # text.panel.DT <- text.panel.DT.original %>% 
        #   select(propertyid, date) %>% 
        #   mutate(rev_date  = date) %>% setDT()
        # cat('# Size of the panel : '  , '\n')
        # print(object.size(text.panel.DT), units = "auto", standard = "SI")
        
        # cat('# Secluding those that will not merge ... ', '\n')
        # text.panel.DT.not_merge <- text.DT[!text.panel.DT, on=c("propertyid", "rev_date")]
        # text.panel.DT.not_merge <- text.panel.DT.not_merge %>% mutate(date = rev_date) %>% setDT()
        
        cat('# Merging rental panel with text data to create text panel ... ', '\n')
        text.panel.DT <- text.DT[text.panel.DT.original, on=c("propertyid")]
        
        # cat('# Rbind additional rows that were not merged ... ' , '\n')
        # text.panel.DT <- rbind(text.panel.DT, text.panel.DT.not_merge)
        
        cat('# Size of the panel + merged text data: '  , '\n')
        print(object.size(text.panel.DT), units = "auto", standard = "SI")
        
        cat('# Removing text.DT ... ', '\n')
        rm(text.DT)
        gc()
        
        cat('# Removing text.panel.DT.not_merge ... ', '\n')
        rm(text.panel.DT.not_merge)
        gc()
        
        cat('# Merging with the rental date panel.', '\n')
        cat('# Time taken : ', '\n')
        print(Sys.time() - time)
        cat('# =============================', '\n')
      }
      
      cat('# Replacing missing with 0', '\n')
      cat('# ------------------------', '\n')
      {
        time <- Sys.time()
        text.panel.DT[is.na(text.panel.DT)] <- 0
        
        cat('# Size of the panel + merged text data + zeros: '  , '\n')
        print(object.size(text.panel.DT), units = "auto", standard = "SI")
        
        cat('# Replacing missing with 0', '\n')
        cat('# Time taken : ', '\n')
        print(Sys.time() - time)
        cat('# ========================', '\n')
      }
      
      cat('# Sorting by propertyid and date for cumsum.' ,'\n')
      cat('# ------------------------------------------' ,'\n')
      {
        time <- Sys.time()
        text.panel.DT <- text.panel.DT[order(propertyid,date)]
        
        cat('# Sorting by propertyid and date for cumsum.' ,'\n')
        cat('# Time taken : ', '\n')
        print(Sys.time() - time)
        cat('# ==========================================' ,'\n')
      }  
      
      # cat('# Calculating the cumulative sum.', '\n')
      # cat('# -------------------------------', '\n')
      # {
      #   time <- Sys.time()
      #   text.panel.DT <- text.panel.DT %>% group_by(propertyid) %>% mutate_at(vars(-propertyid,-rev_date,-date),cumsum)
      #   cat('# No need of rev_date anymore ... ', '\n')
      #   text.panel.DT <- text.panel.DT %>% select(-rev_date) %>% setDT()
      #   
      #   cat('# Size of the panel + merged text data + zeros + cumulation '  , '\n')
      #   print(object.size(text.panel.DT), units = "auto", standard = "SI")
      #   
      #   cat('# Subset by dates ... ', '\n')
      #   text.panel.DT <- text.panel.DT[(date >= "2014-09-01" & date <= "2017-03-31")]
      #   
      #   cat('# Calculating the cumulative sum.', '\n')
      #   cat('# Time taken : ', '\n')
      #   print(Sys.time() - time)
      #   cat('# ===============================', '\n')
      # }
      
      cat('# Merge with Original Text panel.', '\n')
      cat('# -------------------------------', '\n')  
      {
        time <- Sys.time()
        cat('# Left merge with original panel ... ', '\n')
        text.panel.DT <- text.panel.DT[text.panel.DT.original, on=c("propertyid", "date")]
        
        cat('# Size of the final panel data with bed_amenitiess : '  , '\n')
        print(object.size(text.panel.DT), units = "auto", standard = "SI")
        
        cat('# Merge with Original Text panel.', '\n')
        cat('# Time taken : ', '\n')
        print(Sys.time() - time)
        cat('# ===============================', '\n')  
      }
      
      cat('# Final text panel prep and save.', '\n')
      cat('# -------------------------------', '\n')
      {
        time <- Sys.time()
        cat('# Ordering ... ', '\n')
        text.panel.DT <- text.panel.DT[order(propertyid, date)]
        
        {
          cat('# Check consistancy ... ', '\n')
          cat('# Checking rows ... ', '\n')
          print(check_rows)
          cat('# Check propertyid ... ', '\n')
          print(text.panel.DT$propertyid[check_rows])
          cat('# Check date ... ', '\n')
          print(text.panel.DT$date[check_rows])
        }
        
        cat('# Removing propertyid and date ... ', '\n')
        text.panel.DT <- text.panel.DT[ , !c("propertyid", "date")]
        cat('# Writing the csv ... ', '\n')
        fwrite(x = text.panel.DT, 
               file = paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_bed_amenities_panel_DT_", as.character(i), ".csv", sep = ""), 
               append = FALSE)
        rm(text.panel.DT)
        gc()
        cat('# Final text panel prep and save.', '\n')
        cat('# Time taken : ', '\n')
        print(Sys.time() - time)
        cat('# ===============================', '\n')
      }
    }
    cat('# End for loop.', '\n')
    cat('# =============', '\n')
    rm(bed_amenities.tokens.dfm_trim.tfidf)
    gc()
  }  
}


rental.data.desc <- rental.data %>% select(propertyid, desc) %>% setDT()
rental.data.desc <- rental.data.desc[order(propertyid)]

{  
  cat('# Setting up a check for consistancy.', '\n')
  cat('# -----------------------------------', '\n')
  {
    # cat('# Randomly pick 5 observation numbers')
    # check_rows = sample(1:dim(text.panel.DT.original)[1] , size = 5)
    # cat('# Check consistancy of text.panel.DT.original ... ', '\n')
    cat('# Checking rows ... ', '\n')
    print(check_rows)
    cat('# Check propertyid ... ', '\n')
    print(text.panel.DT.original$propertyid[check_rows])
    cat('# Check date ... ', '\n')
    print(text.panel.DT.original$date[check_rows])
    cat('# Check consistancy of demand.data ... ', '\n')
    cat('# Check propertyid ... ', '\n')
    print(demand.data$propertyid[check_rows])
    cat('# Check date ... ', '\n')
    print(demand.data$date[check_rows])
    cat('# Setting up a check for consistancy.', '\n')
    cat('# ===================================', '\n')
  }
  
  cat('# Tokenizing and preprocessing', '\n')
  cat('# ----------------------------', '\n')
  {
    time <- Sys.time()
    cat('# Creating tokens Unigram bigram and bigram with skip ...', '\n')
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
    cat('# Tokenizing and preprocessing', '\n')
    cat('# Time taken : ', '\n')
    print(Sys.time() - time)
    cat('# ============================', '\n')
  }
  
  cat('# Converting to a document feature matrix', '\n')
  cat('# ---------------------------------------', '\n')
  {
    time <- Sys.time()
    desc.tokens.dfm <- dfm(desc.tokens, tolower = FALSE)
    cat('# Initial DFM : ', '\n')
    print(desc.tokens.dfm)
    cat('# Removing desc.tokens ... ', '\n')
    rm(desc.tokens)
    gc()
    cat('# Converting to a document feature matrix', '\n')
    cat('# Time taken : ', '\n')
    print(Sys.time() - time)
    cat('# =======================================', '\n')
  }
  
  cat('# Perform trimming.', '\n')
  cat('# -----------------', '\n')
  {
    time <- Sys.time()
    desc.tokens.dfm_trim <- dfm_trim(desc.tokens.dfm, docfreq_type = "prop",  min_docfreq  = (min.doc.percent/100))
    cat('# DFM after trim (min_docfreq  =', (min.doc.percent/100),') : ', '\n')
    print(desc.tokens.dfm_trim)
    cat('# Removing desc.tokens.dfm ... ', '\n')
    rm(desc.tokens.dfm)
    gc()
    cat('# Perform trimming.', '\n')
    cat('# Time taken : ', '\n')
    print(Sys.time() - time)
    cat('# =================', '\n')
  }
  
  cat('# Compute TF-IDF matrix.','\n')
  cat('# ----------------------','\n')
  {
    time <- Sys.time()
    desc.tokens.dfm_trim.tfidf <- dfm_tfidf(desc.tokens.dfm_trim, scheme_tf = "count", scheme_df = "inverse", base = 10, force = FALSE)
    cat('# DFM after TF_IDF conversion (min_docfreq  =', (min.doc.percent/100),') : ', '\n')
    print(desc.tokens.dfm_trim.tfidf)
    cat('# **# NOTE ***', '\n')
    cat('# This TF-IDF Matrix is still too large to convert to a matrix/data frame.', '\n')
    cat('# So I break them in small objects with 200 features.', '\n')
    cat('# ************', '\n')
    cat('# Removing desc.tokens.dfm_trim ... ', '\n')
    rm(desc.tokens.dfm_trim)
    gc()
    cat('# Compute TF-IDF matrix.','\n')
    cat('# Time taken : ', '\n')
    print(Sys.time() - time)
    cat('# ======================','\n')
  }
  
  cat('# Converting small matrix into rental time panel', '\n')
  cat('# --------------------------------------------------', '\n')
  {
    cat('# Trim again (200 features) after TF-IDF to make convertible matrix', '\n')
    
    cat('# Begin for loop.', '\n')
    cat('# ---------------', '\n')
    for(i in seq(from = 1, to = nfeat(desc.tokens.dfm_trim.tfidf), by = 200)){
      # i=1
      cat('# For loop begins ... ', '\n')
      cat('# Features from ', i , ' to ', (i+199), ' ... ' ,'\n')
      
      {
        cat('# Trimming ... ', '\n')
        tfidf_trim200 <- dfm_trim(desc.tokens.dfm_trim.tfidf, docfreq_type = "rank", max_docfreq = i , min_docfreq = (i+199))
        cat('# DFM TF IDF object after trim : ', '\n')
        print(tfidf_trim200)
      }
      
      if (nfeat(tfidf_trim200) == 0) {
        cat('# Breaking the loop at i = ', i,'\n')
        rm(tfidf_trim200)
        gc()
        break
      }
      
      cat('# Convert to a data table.', '\n')
      cat('# ------------------------', '\n')
      {
        time <- Sys.time()
        cat('# Converting to df ... ', '\n')
        text.DT <- convert(tfidf_trim200, to = "data.frame") %>% setDT()
        cat('# Renaming all text variables to end with _text ... ', '\n')
        names(text.DT) <- paste(colnames(text.DT), "desc", sep="_")
        cat('# Removing tfidf_trim200 ... ', '\n')
        rm(tfidf_trim200)
        gc()
        cat('# Convert to a data table.','\n')
        cat('# Time taken : ', '\n')
        print(Sys.time() - time)
        cat('# ========================','\n')
      }
      
      
      cat('# Combine with original to keep the rental information.','\n')
      cat('# --------------------------------------------------------------','\n')
      {
        time <- Sys.time()
        cat('# Combining text data table to original desc data table ... ', '\n')
        text.DT <- cbind(rental.data.desc, text.DT)
        cat('# Remove columns : descer_id , id , descer_name , desc , document ... ', '\n')
        text.DT <- text.DT[ , -c("desc","document_desc")]
        # cat('# Collect descs that are posted on same day ... ', '\n')
        # text.DT <- text.DT[ , lapply(.SD,sum, na.rm=TRUE),by=c("propertyid", "rev_date")]
        # text.DT <- text.DT %>%
        #   select(-descer_id, -id,-descer_name,-desc,-document) %>% 
        #   group_by(propertyid , rev_date, date) %>% 
        #   summarize_all(list(textvar = sum),  na.rm = TRUE)
        
        cat('# Combine with original to keep the rental and date information.','\n')
        cat('# Time taken : ', '\n')
        print(Sys.time() - time)
        cat('# ==============================================================','\n')
      }
      
      cat('# Merging with the rental date panel.', '\n')
      cat('# -----------------------------------', '\n')
      {
        time <- Sys.time()
        
        # cat('# Creating text panel with variable rev_date to merge on (can be changed based on booking date) ... ')
        # text.panel.DT <- text.panel.DT.original %>% 
        #   select(propertyid, date) %>% 
        #   mutate(rev_date  = date) %>% setDT()
        # cat('# Size of the panel : '  , '\n')
        # print(object.size(text.panel.DT), units = "auto", standard = "SI")
        
        # cat('# Secluding those that will not merge ... ', '\n')
        # text.panel.DT.not_merge <- text.DT[!text.panel.DT, on=c("propertyid", "rev_date")]
        # text.panel.DT.not_merge <- text.panel.DT.not_merge %>% mutate(date = rev_date) %>% setDT()
        
        cat('# Merging rental panel with text data to create text panel ... ', '\n')
        text.panel.DT <- text.DT[text.panel.DT.original, on=c("propertyid")]
        
        # cat('# Rbind additional rows that were not merged ... ' , '\n')
        # text.panel.DT <- rbind(text.panel.DT, text.panel.DT.not_merge)
        
        cat('# Size of the panel + merged text data: '  , '\n')
        print(object.size(text.panel.DT), units = "auto", standard = "SI")
        
        cat('# Removing text.DT ... ', '\n')
        rm(text.DT)
        gc()
        
        cat('# Removing text.panel.DT.not_merge ... ', '\n')
        rm(text.panel.DT.not_merge)
        gc()
        
        cat('# Merging with the rental date panel.', '\n')
        cat('# Time taken : ', '\n')
        print(Sys.time() - time)
        cat('# =============================', '\n')
      }
      
      cat('# Replacing missing with 0', '\n')
      cat('# ------------------------', '\n')
      {
        time <- Sys.time()
        text.panel.DT[is.na(text.panel.DT)] <- 0
        
        cat('# Size of the panel + merged text data + zeros: '  , '\n')
        print(object.size(text.panel.DT), units = "auto", standard = "SI")
        
        cat('# Replacing missing with 0', '\n')
        cat('# Time taken : ', '\n')
        print(Sys.time() - time)
        cat('# ========================', '\n')
      }
      
      cat('# Sorting by propertyid and date for cumsum.' ,'\n')
      cat('# ------------------------------------------' ,'\n')
      {
        time <- Sys.time()
        text.panel.DT <- text.panel.DT[order(propertyid,date)]
        
        cat('# Sorting by propertyid and date for cumsum.' ,'\n')
        cat('# Time taken : ', '\n')
        print(Sys.time() - time)
        cat('# ==========================================' ,'\n')
      }  
      
      # cat('# Calculating the cumulative sum.', '\n')
      # cat('# -------------------------------', '\n')
      # {
      #   time <- Sys.time()
      #   text.panel.DT <- text.panel.DT %>% group_by(propertyid) %>% mutate_at(vars(-propertyid,-rev_date,-date),cumsum)
      #   cat('# No need of rev_date anymore ... ', '\n')
      #   text.panel.DT <- text.panel.DT %>% select(-rev_date) %>% setDT()
      #   
      #   cat('# Size of the panel + merged text data + zeros + cumulation '  , '\n')
      #   print(object.size(text.panel.DT), units = "auto", standard = "SI")
      #   
      #   cat('# Subset by dates ... ', '\n')
      #   text.panel.DT <- text.panel.DT[(date >= "2014-09-01" & date <= "2017-03-31")]
      #   
      #   cat('# Calculating the cumulative sum.', '\n')
      #   cat('# Time taken : ', '\n')
      #   print(Sys.time() - time)
      #   cat('# ===============================', '\n')
      # }
      
      cat('# Merge with Original Text panel.', '\n')
      cat('# -------------------------------', '\n')  
      {
        time <- Sys.time()
        cat('# Left merge with original panel ... ', '\n')
        text.panel.DT <- text.panel.DT[text.panel.DT.original, on=c("propertyid", "date")]
        
        cat('# Size of the final panel data with descs : '  , '\n')
        print(object.size(text.panel.DT), units = "auto", standard = "SI")
        
        cat('# Merge with Original Text panel.', '\n')
        cat('# Time taken : ', '\n')
        print(Sys.time() - time)
        cat('# ===============================', '\n')  
      }
      
      cat('# Final text panel prep and save.', '\n')
      cat('# -------------------------------', '\n')
      {
        time <- Sys.time()
        cat('# Ordering ... ', '\n')
        text.panel.DT <- text.panel.DT[order(propertyid, date)]
        
        {
          cat('# Check consistancy ... ', '\n')
          cat('# Checking rows ... ', '\n')
          print(check_rows)
          cat('# Check propertyid ... ', '\n')
          print(text.panel.DT$propertyid[check_rows])
          cat('# Check date ... ', '\n')
          print(text.panel.DT$date[check_rows])
        }
        
        cat('# Removing propertyid and date ... ', '\n')
        text.panel.DT <- text.panel.DT[ , !c("propertyid", "date")]
        cat('# Writing the csv ... ', '\n')
        fwrite(x = text.panel.DT, 
               file = paste(project.path, "Output/TEMP/ForH2O_DML/", "21_01_desc_panel_DT_", as.character(i), ".csv", sep = ""), 
               append = FALSE)
        rm(text.panel.DT)
        gc()
        cat('# Final text panel prep and save.', '\n')
        cat('# Time taken : ', '\n')
        print(Sys.time() - time)
        cat('# ===============================', '\n')
      }
    }
    cat('# End for loop.', '\n')
    cat('# =============', '\n')
    rm(desc.tokens.dfm_trim.tfidf)
    gc()
  }  
}