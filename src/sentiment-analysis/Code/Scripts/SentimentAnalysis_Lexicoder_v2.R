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


setwd("~/CloudStation/UZH/Research Applications/Social Science One/Code")
#################################################
###### 1) Data Import
load("corpus-nobillag.RDS")

corp$search.term <- "no billag"

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
    text <- data[i,'txt.merged.utf8']
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


    
    data_preptext <- cbind(data[i, 'search.term'],data[i,'txt.merged.utf8'],data[i,'language'], text) 
    colnames(data_preptext)[ncol(data_preptext)] <- "processed.text"
    data_preptext$processed.text <- as.character(data_preptext$processed.text)
    
    data_preptext

    
 }
  return(res)
  on.exit(stopCluster(clu))
  stopCluster(clu)
}
processedtext <- processdata(corp, 3, stopwordlist)

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
  cat("Make sure you have loaded the 'auto_dictionaries_lsd.RData' in the environment. Without them the function will not work\n")
  clu <- makeCluster(nodes)
  registerDoParallel(clu)
  data <- data %>% filter(language %in% c("german", "french", "italian", "english"))
  numoit <- nrow(data)
  result <- foreach(i = 1:numoit, combine = rbind, 
                    .packages = c("dplyr", "tm", "quanteda", "stringr", "tokenizers"), 
                    .export = c("extendeddict_de", "extendeddict_fr", "extendeddict_it", "extendeddict_en")) %dopar% {
    lang <- data[i,'language']
    if(lang == "french"){
      dict_language <- "fr"
    } else if (lang == "italian"){
      dict_language <- "it"
    } else if (lang == "english") {
      dict_language <- "en"
    } else if (lang == "german") {
      dict_language <- "de"
    } else {
      dict_language <- "invalid"
    }
    if(dict_language == "invalid"){
      cat("Text of invalid Language!\n")
    } else {
      protex <- data[i,'processed.text']
      sentisave <- convert(dfm(protex, remove_punct = T, remove_numbers = T, dictionary = get(paste0("extendeddict_",dict_language))), to = "data.frame")
      senttex <- log((sentisave$pos+0.5)/(sentisave$neg+0.5))
      senttex <- ifelse(identical(senttex, numeric(0)), 0, senttex) #Zero Sentiment Exeption
      
      
      resdat <- cbind(data[i,], senttex)
      colnames(resdat)[ncol(resdat)] <- "Sentiment_Value"
      resdat
      
    }
                    }
  
  return(result)
  on.exit(stopCluster(clu))
  stopCluster(clu)
}


final_dat <- sentimentfun(processedtext, dictionaries,  nodes = 2)

#library(microbenchmark) (performs pretty good.)
#microbenchmark(sentimentfun(processedtext, dictionaries,  nodes = 2), 
              # sentimentfun(processedtext, dictionaries,  nodes = 1), times = 10)


df <- data.frame(matrix(unlist(final_dat), nrow=length(final_dat), byrow=T), stringsAsFactors = F)
df[,2] <- NULL
colnames(df) <- c("search.term", "language", "text", "sentiment")
df$sentiment <- as.numeric(df$sentiment)

##### Some first descriptive stats about the URLs
library(ggplot2)
dfplot <- df %>% group_by(language) %>% summarize(Avg = mean(sentiment, na.rm = T), SD = sd(sentiment, na.rm = T))

ggplot(dfplot, aes(x=language, y=Avg), color = "blue") +
  geom_pointrange(aes(ymin = Avg-SD, ymax = Avg+SD), color = "lightblue") +
  labs(title = "Difference in language", y = "Average Sentiment", x ="Texts language") +
  theme_classic()
