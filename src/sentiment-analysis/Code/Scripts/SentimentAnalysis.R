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

setwd("~/CloudStation/UZH/Research Applications/Social Science One/Code")
#################################################
###### 1) Data Import
data <- read_csv("All-Text-Files.csv")


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
  listout <- list()
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
    text <- tm::tm_map(text, tm::removeWords, stopwordlist)
    preptext <- text[["character(0)"]][["content"]]
    
    words <- tokenize_words(preptext)
    names(words) <- c("words")
    
    data_preptext <- cbind(data[i,], preptext)
    colnames(data_preptext)[7] <- "Processed_Text"
    data_preptext$`Processed_Text` <- as.character(data_preptext$`Processed_Text`)
    
    
    sublist <- list(data_preptext, words)
    listout[[i]] <- list(sublist)

    
 }
  return(res)
  stopCluster(clu)
}
processedtext <- processdata(data, 3, stopwordlist)

#################################################
###### 3) Sentiment Analysis

#Load sentiWS data
sent <- c(
  # positive words
  readLines("SentiWS_v1.8c_Positive.txt",
            encoding = "UTF-8"),
  # negative words
  readLines("SentiWS_v1.8c_Negative.txt",
            encoding = "UTF-8")
) %>% lapply(function(x) {
  # Extract some columns
  res <- strsplit(x, "\t", fixed = TRUE)[[1]]
  return(data.frame(words = res[1], value = res[2], 
                    stringsAsFactors = FALSE))
}) %>%
  bind_rows %>% 
  mutate(words = gsub("\\|.*", "", words) %>% tolower,
         value = as.numeric(value)) %>% 
  # calculate average for word which are recurring more than once 
  group_by(words) %>% summarise(value = mean(value)) %>% ungroup


# Get Sentiment for Texts while filtering words which are neither positive nor negative:
sentimentfun <- function(processedtext, sentimentdf, nodes = 1){
  clu = makeCluster(nodes)
  registerDoParallel(clu)
  listout <-list()
  res <- foreach(i = 1:length(processedtext), .combine = "cbind", .packages = c("dplyr", "tm", "stringr", "tokenizers")) %dopar% {
    words <- processedtext[[i]][[2]]
    txt <- as.data.frame(words, stringsAsFactors = FALSE)

    
    sentTxt <- left_join(txt, sentimentdf, by = "words") %>% 
      mutate(value = as.numeric(value)) %>% 
      filter(!is.na(value))
    
    averagesent <- mean(sentTxt$value)
    
    data <- as.data.frame(processedtext[[i]][1])
    data$'Mean_Sentiment' <- averagesent
    
    words <- as.data.frame(processedtext[[i]][2])
    words$words <- as.character(words$words)
    
    sublist <- list(data, words, sentTxt)
    listout[[i]] <- list(sublist)
    
  }
  return(res)
  stopCluster(clu)
} 

## Check if it is faster with more cores:
start <- Sys.time()
fin <- sentimentfun(processedtext, sent, nodes =3)
stop <- Sys.time()
stop - start 

start <- Sys.time()
fin <- sentimentfun(processedtext, sent, nodes =1)
stop <- Sys.time()
stop - start 
## With 10'000 Texts it is about 25% faster with 3 instead of one core.....