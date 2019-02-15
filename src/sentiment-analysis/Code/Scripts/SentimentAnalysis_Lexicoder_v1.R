#################################################
##### Sentiment Analysis with SentiWS
#################################################
##### Content:
# 1) Data Import
# 2) Pre-Processing of Text
# 3) Sentiment Anslysis
#################################################
##### Libraries used:
library(tm)
library(lsa)
library(tokenizers)
library(dplyr)
library(stringi)
library(stringr)
library(foreach)
library(doParallel)
library(readr)
library(quanteda)
library(cld2)

setwd("~/CloudStation/UZH/Research Applications/Social Science One/Code")
#################################################
###### 1) Data Import
data <- read_csv("All-Text-Files.csv")
data <- head(data, 400)

#################################################
###### 2) Pre-Processing 

# Stopwords which should be removed including your own stopwords.
stopwordtm <- stopwords("german")
stopwordls <- stopwords_de
stopwordex <- c()
stopwordlist <- c(words = unique(c(stopwordex, stopwordls, stopwordtm)))

# Function which pre-processes text for later analysis requires tm, stringr and dyplr
processdata <- function(data, nodes = 1, stopwordvector){
  clu = makeCluster(nodes)
  registerDoParallel(clu)
  stopwordlist <- stopwordvector

 res <-  foreach(i = 1:nrow(data), .combine = "rbind", .packages = c("dplyr", "tm", "stringr", "tokenizers")) %dopar% {
    text <- data[i,'Text']
    text <- text %>% gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", ., perl=TRUE) %>%
            gsub("\n", " ", .) %>%
            gsub("- ", "", .) %>%
            str_replace_all(., "[[:digit:]]", "")
    
    text <- tm::VCorpus(tm::VectorSource(text))
    text <- tm::tm_map(text, tm::removeNumbers)
    text <- tm::tm_map(text, tm::content_transformer(tolower))
    text <- tm::tm_map(text, tm::removePunctuation)
    text <- tm::tm_map(text, tm::stripWhitespace)
    text <- tm::tm_map(text, tm::PlainTextDocument)
    #text <- tm::tm_map(text, tm::removeWords, stopwordlist)
    text <- text[["character(0)"]][["content"]]


    
    data_preptext <- cbind(data[i,], text)
    colnames(data_preptext)[7] <- "Processed_Text"
    data_preptext$Text <- NULL
    data_preptext$`Processed_Text` <- as.character(data_preptext$`Processed_Text`)
    
    data_preptext

    
 }
  return(res)
  stopCluster(clu)
}
processedtext <- processdata(data, 3, stopwordlist)

#################################################
###### 3) Sentiment Analysis
# Load Dictionary from Proksch et al. Multilingual Sentiment analysis (based on the Lexicoder Dictionary)
load("auto_dictionaries_lsd.RData")
dictionaries <- load("auto_dictionaries_lsd.RData")
# Load englisch Dictionary too (requires quanteda):
extendeddict_en <- data_dictionary_LSD2015
# Remove negated things from this dictionary...
extendeddict_en[["neg_negative"]] <- NULL
extendeddict_en[["neg_positive"]] <- NULL

languages <- c("de", "it", "fr")


sentimentfun <- function(data, dictionarieslist = dictionaries,  nodes = 1){
  cat("Make sure you have loaded the 'auto_dictionaries_lsd.RData' in the environment.\nWithout them the function will not work")
  clu <- makeCluster(nodes)
  registerDoParallel(clu)
  numoit <- nrow(data)
  result <- foreach(i = 1:numoit, combine = rbind, 
                    .packages = c("dplyr", "tm", "quanteda", "stringr", "tokenizers", "cld2"), 
                    .export = c("extendeddict_de", "extendeddict_fr", "extendeddict_it", "extendeddict_en")) %dopar% {
    lang <- detect_language_mixed(data[i,'Processed_Text'])
    lang_cert <- lang[["classificaton"]][["proportion"]][1]
    lang <- lang[["classificaton"]][1, 2]
    if(lang == "fr"){
      dict_language <- "fr"
    } else if (lang == "it"){
      dict_language <- "it"
    } else if (lang == "en") {
      dict_language <- "en"
    } else {
      dict_language <- "de"
    }
    protex <- data[i,'Processed_Text']
    sentisave <- convert(dfm(protex, remove_punct = T, remove_numbers = T, dictionary = get(paste0("extendeddict_",dict_language))), to = "data.frame")
    senttex <- log((sentisave$pos+0.5)/(sentisave$neg+0.5))
    senttex <- ifelse(identical(senttex, numeric(0)), 0, senttex) #Zero Sentiment Exeption
    
    
    resdat <- cbind(data[i,], senttex, lang_cert, lang)
    colnames(resdat)[ncol(resdat)-2] <- "Sentiment_Value"
    colnames(resdat)[ncol(resdat)-1] <- "Languagae Certainty"
    colnames(resdat)[ncol(resdat)] <- "Languagae"
    resdat$Languagae <- as.character(resdat$Languagae)
    resdat
     

  }
  return(result)
  stopCluster(clu)
}


final_dat <- sentimentfun(processedtext, dictionaries,  nodes = 2)

#df <- data.frame(matrix(unlist(final_dat), nrow=length(final_dat), byrow=T), stringsAsFactors = F)

