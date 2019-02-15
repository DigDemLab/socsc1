##################################################################################################
# Sentiment Analysis with Lexicoder
##################################################################################################
# Description:
##################################################################################################
# Script to get sentiment from snowballed texts related to 11 national polls in switzerland 
# including several descriptive plots around the sentiment regarding the polls, language and 
# different newspapers
# We use a method proposed by Proksch et al. (2018)
##################################################################################################
# Content:
##################################################################################################
# 1) Dependencies
# 2) Data Import
# 3) Pre-Processing of Text
# 4) Sentiment Anslysis
# 5) Data Transformations for Descriptives
# 6) Some first descriptive stats about the URLs
# 7) Term-Frequencies
# 8) # Sentiment Differences from polls between papers
##################################################################################################
# 1) Dependencies: 
##################################################################################################
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
library(ggsci)
##################################################################################################
# 2) Data Import
##################################################################################################
setwd("~/DigDemLab/Research Applications/Social Science One/Code")

load("CORPUS.RDS")

corpus <- corpus[!duplicated(corpus$url),]
##################################################################################################
# 3) Pre-Processing 
##################################################################################################
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


    
    data_preptext <- cbind(data[i, 'url'], data[i, 'cat'], data[i, 'search.term'],data[i,'txt.merged.utf8'],data[i,'language'], text) 
    colnames(data_preptext)[ncol(data_preptext)] <- "processed.text"
    data_preptext$processed.text <- as.character(data_preptext$processed.text)
    
    data_preptext

    
 }
  return(res)
 on.exit(parallel::stopCluster(clu))
 stopCluster(clu)
}


processedtext <- processdata(corpus, 3, stopwordlist)

##################################################################################################
# 4) Sentiment Analysis
##################################################################################################
# Load Dictionary from Proksch et al. Multilingual Sentiment analysis (based on the Lexicoder Dictionary)
load("auto_dictionaries_lsd.RData")
dictionaries <- load("auto_dictionaries_lsd.RData")

# Load improved Lexicons
load("lsde_frenche_germane.RData")
dict2 <- load("lsde_frenche_germane.RData")

#Cobine Vector
dictionaries <- c(dictionaries, dict2)

sentimentfun <- function(data, dictionarieslist = dictionaries,  nodes = 1){
  cat("Make sure you have loaded the 'auto_dictionaries_lsd.RData' and 'lsde_frenche_germane.RData' in the environment.
      \nWithout them the function will not work\n")
  clu <- makeCluster(nodes)
  registerDoParallel(clu)
  data <- data %>% filter(language %in% c("german", "french", "italian", "english"))
  reslist <- list()
  numoit <- nrow(data)
  
  #Helping Function:
  replifun <- function(x) {
    c <- c()
    if(ncol(x)<=1){
      c <- c(NA)
    } else {
      for(j in 2:ncol(x)) {
        res <-  replicate(x[1,j], colnames(x)[j])
        res <- paste(res, collapse = " ")
        
        c <- c(c, res)
        
        c <- paste(c, collapse = " ")
      }
    }
    return(c)    
  }
  
  result <- foreach(i = 1:numoit, combine = rbind, 
                    .packages = c("dplyr", "tm", "quanteda", "stringr", "tokenizers", "stringi"), 
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
        
        dict_lang_pos_neg <-  get(paste0("extendeddict_",dict_language))
        dict_lang_pos <- dict_lang_pos_neg[[1]]
        dict_lang_neg <- dict_lang_pos_neg[[2]]
        
        
        #Calculate Sentiment of Text: 
        sentisave <- convert(dfm(protex, remove_punct = T, remove_numbers = T, dictionary = dict_lang_pos_neg), to = "data.frame")
        senttex <- log((sentisave$pos+0.5)/(sentisave$neg+0.5))
        senttex <- ifelse(identical(senttex, numeric(0)), 0, senttex) #Zero Sentiment Exeption
        
        #Extract Keywords found in Text:
        senti_words_pos_pre <- convert(dfm(protex, select = dict_lang_pos, verbose = FALSE), to = "data.frame")
        senti_words_neg_pre <- convert(dfm(protex, select = dict_lang_neg, verbose = FALSE), to = "data.frame")
        
        
        senti_words_pos <- replifun(senti_words_pos_pre)
        senti_words_neg <- replifun(senti_words_neg_pre)
        
        #Combining all to a List        
        resdat <- cbind(data[i,], senttex, senti_words_pos, senti_words_neg)
        colnames(resdat)[ncol(resdat)-2] <- "Sentiment_Value"
        resdat$senti_words_pos <- as.character(resdat$senti_words_pos)
        resdat$senti_words_neg <- as.character(resdat$senti_words_neg)
        resdat
        
      }
    }
  return(result)
  on.exit(parallel::stopCluster(clu))
  stopCluster(clu)
}


final_dat <- sentimentfun(processedtext, dictionaries,  nodes = 3)
##################################################################################################
# 5) Data Transformations for Descriptives: 
##################################################################################################
df <- data.frame(matrix(unlist(final_dat), nrow=length(final_dat), byrow=T), stringsAsFactors = F)
df[,4] <- NULL
colnames(df) <- c("url", "cat", "search.term", "language", "text", "sentiment", "pos_words", "neg_words")
df$sentiment <- as.numeric(df$sentiment)

df$poll<- as.factor(df$cat)
a <- levels(df$poll)
level_key <- list('Bundesbeschluss vom 14.03.2017 über die Ernährungssicherheit (direkter Gegenentwurf zur' = "Ernährungssicherheit", 
                  'Bundesbeschluss vom 14.03.2017 über die Ernährungssicherheit (direkter Gegenentwurf zur Volksinitiative «Für Ernährungssicherheit») (24.09.2017)' = "Ernährungssicherheit",
                  'Bundesbeschluss vom 16.06.2017 über die neue Finanzordnung 2021 (04.03.2018)' = "Finanzordnung 2021", 
                  'Bundesbeschluss vom 17.03.2017 über die Zusatzfinanzierung der AHV durch eine Erhöhung der Mehrwertsteuer (24.09.2017)' = "Mehrwertsteuer",
                  'Bundesbeschluss vom 30.09.2016 über die erleichterte Einbürgerung von Personen der dritten Ausländergeneration (12.02.2017)' = "Erl. Einbürgerung.", 
                  'Bundesbeschluss vom 30.09.2016 über die Schaffung eines Fonds für die Nationalstrassen und den Agglomerationsverkehr (12.02.2017)' = "NAF", 
                  'Bundesgesetz vom 17.03.2017 über die Reform der Altersvorsorge 2020 (24.09.2017)' = "Altersvorsorge 2020",
                  'Bundesgesetz vom 17.06.2016 über steuerliche Massnahmen zur Stärkung der Wettbewerbsfähigkeit des Unternehmensstandortes Schweiz (Unternehmenssteruerreformgesetz III) (12.02.2017)' = "USR III",
                  'Bundesgesetz vom 17.06.2016 über steuerliche Massnahmen zur Stärkung der (Unternehmenssteruerreformgesetz III) (12.02.2017)' = "USR III",
                  'Bundesgesetz vom 29.09.2017 über Geldspiele (Geldspielgesetz, BGS) (10.06.2018)' = "Geldspielgesetz", 
                  'Energiegesetz (EnG) vom 30.09.2016 (21.05.2017)' = "EnG", 
                  'Volksinitiative vom 01.12.2015 «Für krisensicheres Geld: Geldschöpfung allein durch die Nationalbank! (Vollgeld-Initiative)» (10.06.2018)' = "Vollgeld",
                  'Volksinitiative vom 01.12.2015 «Für krisensicheres Geld: Geldschöpfung allein durch die' = "Vollgeld",
                  'Volksinitiative vom 11.12.2015 «Ja zur Abschaffung der Radio- und Fernsehgebühren (Abschaffung der Billag-Gebühren)» (04.03.2018)' = "no-billag")
df$poll <- recode_factor(df$poll, !!!level_key)

df$initiative <- "Referenda"
df$initiative[df$poll %in% c("Vollgeld", "no-billag")] <- "Initiative" 

saveRDS(final_dat, "Corpus_Sentiment.rds")
write_csv(df, "Corpus_Sentiment_df.csv")

setwd("~/DigDemLab/Research Applications/Social Science One/Code/Output")
dfall <- read_csv("Corpus_Sentiment_df.csv")
df <- dfall
###################################################################################################
# 6) Some first descriptive stats about the URLs
###################################################################################################
library(ggplot2)
library(ggsci)
library(cowplot)
library(ggrepel)
se <- function(x) sqrt(var(x)/length(x))


#Language Variation
dfplotlang <- dfall %>% group_by(language) %>% summarize(Avg = mean(sentiment, na.rm = T), SE = se(sentiment), SUM  = n())

a <- ggplot(dfplotlang) +
        geom_point(aes(x=language, y=Avg), size = 2, shape = 21, fill = "black") +
        geom_errorbar(aes(x = language, ymin = Avg-SE, ymax = Avg+SE), color = "blue", width = .2) +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        geom_text(aes(y = Avg-SE, x = language, label = paste0("n = ",SUM)),vjust = 1.3, hjust = 0.5) +
        geom_text(aes(y = -.48, x = 3.9, label = paste0("Number of Texts: ", sum(SUM)))) +
        labs(title = "Variation of Sentiment by Language:", y = "Average Sentiment", x ="Texts language") +
        scale_y_continuous(limits = c(-.25,1)) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank()) 

ggsave("Sent_Diff_by_Lang.png", a, device = "png", width = 4, height = 6, dpi = 300)
ggsave("Sent_Diff_by_Lang.pdf", a, device = "pdf", width = 4, height = 6, dpi = 300)

#Avergae Sentiment of all Polls in German and Fernch only:
df <- filter(df, language %in% c("german", "french", "italian"))
dfplotpoll <- df %>% group_by(poll, language) %>% summarize(Avg = mean(sentiment, na.rm = T), SE = se(sentiment), SUM  = n())

b <- ggplot(dfplotpoll) +
        geom_point(aes(x=poll, y=Avg, color = language, fill = language), size = 2, shape = 21,  position=position_dodge(width = 0.75)) +
        geom_errorbar(aes(x = poll, ymin = Avg-SE, ymax = Avg+SE, color = language), width = .2,  position=position_dodge(width = 0.75)) +
        geom_hline(yintercept = 0, color = "dark grey", linetype = "dashed") +
        geom_text(aes(y = Avg-(SE+.13), x = poll, label = paste0("n = ",SUM), color = language), position=position_dodge(width = 0.75), vjust = .5, hjust = 0.5) +
        geom_text(aes(y = .55, x = 0.75, label = paste0("Number of Texts: ", sum(SUM)))) +
        labs(title = "Variation of Sentiment by polls:", y = "Average Sentiment", x ="Poll") +
        scale_y_continuous(limits = c(-.85,.85)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank()) +
        scale_fill_startrek(name="Language:",
                        breaks=c("german", "french", "italian"),
                        labels=c("German", "French", "Italian")) +
        scale_colour_startrek() +
        theme(legend.position="bottom",legend.direction="horizontal") +
        coord_flip() +
        guides(color=FALSE) 
        

ggsave("Sent_Diff_by_Poll.png", b, device = "png", width = 6, height = 9, dpi = 300)
ggsave("Sent_Diff_by_Poll.pdf", b, device = "pdf", width = 6, height = 9, dpi = 300)

#Average Sentiment difference between Initiatives and Referendas in German only 
dfplotinitiative <- df %>% group_by(initiative) %>% summarize(Avg = mean(sentiment, na.rm = T), SE = se(sentiment), SUM  = n())

c <- ggplot(dfplotinitiative) +
        geom_point(aes(x=initiative, y=Avg), size = 2, shape = 21, fill = "black") +
        geom_errorbar(aes(x = initiative, ymin = Avg-SE, ymax = Avg+SE), color = "blue", width = .1) +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        geom_text(aes(y = Avg-SE, x = initiative, label = paste0("n = ",SUM)),vjust = 1.3, hjust = 0.5) +
        geom_text(aes(y = -.01, x = 2, label = paste0("Number of Texts: ", sum(SUM)))) +
        labs(title = "Variation of Sentiment between\nInitiatives and Referenda", y = "Average Sentiment", x ="Poll Type") +
        scale_y_continuous(limits = c(-0.01,.3)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank())

ggsave("Sent_Diff_by_Poll_Type.png", c, device = "png", width = 4, height = 8, dpi = 300)
ggsave("Sent_Diff_by_Poll_Type.pdf", c, device = "pdf", width = 4, height = 8, dpi = 300)

#Distribution of sentiment
d <- ggplot(df, aes(x=sentiment)) + 
        geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") +
        geom_density(alpha=.4, fill="#df0000") +
        labs(title = "Distribution of Sentiment", y = "Density ", x ="Sentiment") +
        theme_bw()

ggsave("Sent_Dist.png", d, device = "png", width = 8, height = 6, dpi = 300)
ggsave("Sent_Dist.pdf", d, device = "pdf", width = 8, height = 6, dpi = 300)

#Distribution of sentiment by poll
dfsentpol <- df %>% group_by(poll) %>% summarize(Avg = mean(sentiment, na.rm = T), SE = se(sentiment), SUM  = n())

e <- ggplot(df, aes(x=sentiment)) + 
        geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") +
        geom_density(alpha=.4, fill="#df0000") +
        labs(title = "Distribution of Sentiment", y = "Density ", x ="Sentiment [neg. - pos.]") +
        geom_text(data = dfsentpol, aes(y = .9, x = 2, label = paste0("n = ",SUM))) +
        facet_wrap(~poll) +
        theme_bw()

ggsave("Sent_Dist_by_Poll.png", e, device = "png", width = 8, height = 6, dpi = 300)
ggsave("Sent_Dist_by_Poll.pdf", e, device = "pdf", width = 8, height = 6, dpi = 300)

#Distribution of sentiment between referenda and initiative
f <- ggplot(df, aes(x=sentiment)) + 
        geom_histogram(aes(y=..density.., fill=initiative), alpha=.5, binwidth=.5, colour="black", position="dodge") +
        geom_density(aes(fill=initiative, color=initiative), alpha=.3) +
        labs(title = "Distribution of Sentiment", y = "Density ", x ="Sentiment [neg. - pos.]") +
        theme_bw() +
        geom_text(data = dfplotinitiative, aes(y = .8, x = 2, label = paste0("n (Initiatives) = ",SUM[1],"\nn (Referenda) = ", SUM[2]))) +
        scale_fill_startrek(name="Poll Type:",
                         breaks=c("Initiative", "Referenda"),
                         labels=c("Initiative", "Referenda")) +
        scale_colour_startrek() +
        theme(legend.position="bottom",legend.direction="horizontal") +
        guides(color=FALSE)

ggsave("Sent_Dist_by_Poll_Type.png", f, device = "png", width = 8, height = 6, dpi = 300)
ggsave("Sent_Dist_by_Poll_Type.pdf", f, device = "pdf", width = 8, height = 6, dpi = 300)

### Table of Sentiment
library(stargazer)
table <- df %>% group_by(poll) %>% summarize(Avg = mean(sentiment, na.rm = T), SE = se(sentiment), SUM  = n())
colnames(table) <- c("Name of the Poll:", "Average Sentiment", "Standard Error", "Number of URLs")
table$'Name of the Poll:' <- as.character(table$'Name of the Poll:')
table[,2:3] <- round(table[,2:3], digits = 3)

stargazer(table, type = "text", header = F, summary = FALSE, out = "sentiment_polls_table.txt") 

###################################################################################################
# 7) Term-Frequencies
###################################################################################################
# Most freuqent positive and negative terms per poll and over all polls by type in german 
langc <- c("german", "french", "italian")
for(l in 1:length(langc)){  
  dffq <- filter(df, language == langc[l])

  top_pos_words_polls <- textstat_frequency(dfm(dffq$pos_words), n = 10, groups = dffq$poll)
  top_neg_words_polls <- textstat_frequency(dfm(dffq$neg_words), n = 10, groups = dffq$poll)
  
  top_pos_words_type <- textstat_frequency(dfm(dffq$pos_words), n = 10, groups = dffq$initiative)
  top_neg_words_type <- textstat_frequency(dfm(dffq$neg_words), n = 10, groups = dffq$initiative)
  
  top_pos_words <- textstat_frequency(dfm(dffq$pos_words), n = 10)
  top_neg_words <- textstat_frequency(dfm(dffq$neg_words), n = 10)
  
  
  ### Plot for overall...
  g <- ggplot(top_pos_words) +
    geom_bar(aes(x = reorder(feature, frequency), y = frequency), stat="identity", color = "#0099f6", fill = "#0099f6") + 
    labs(title = "Most frequent postive words:", y = "Frequency (over all texts)", x = "Word") +
    theme_bw() +
    coord_flip()
  g
  h <- ggplot(top_neg_words) +
    geom_bar(aes(x = reorder(feature, frequency), y = frequency), stat="identity", color = "#df0000", fill = "#df0000") + 
    labs(title = "Most frequent negative words:", y = "Frequency (over all texts)", x = "Word") +
    theme_bw() +
    coord_flip()
  h
  gh <- cowplot::plot_grid(g,h, align = "h", axis = "l")
  title <- cowplot::ggdraw() + 
            cowplot::draw_label(paste0("Most frequent positive and negative words in all ", langc[l], " URLs"))
  
  gh <- cowplot::plot_grid(title, gh, ncol = 1, rel_heights = c(0.1,1))
  gh
  ggsave(paste0("Word_Freq_", langc[l], ".png"), gh, device = "png", width = 8, height = 6, dpi = 300)
}


# 100 Most frequent Terms in Texts (Check to see if there are still things in the text which we do not like...)
most_freq_words_by_poll <- textstat_frequency(dfm(dfall$text), n = 200, groups = dfall$poll)
most_freq_words <- textstat_frequency(dfm(dfall$text), n = 200)

most_freq_words$group <- NULL

write.table(most_freq_words,"most_freq_words.txt",sep="\t",row.names=FALSE)
write.table(most_freq_words_by_poll,"most_freq_words_by_poll.txt",sep="\t",row.names=FALSE)

###################################################################################################
# 8) # Sentiment Differences from polls between papers
###################################################################################################
# Sentiment Difference between Papers in a particular initiative or referenda
dfall$paper <- NA

search_terms <- c("woz", "watson", "taz", "weltwoche", "nzz", "blick", "republik", "tagesanzeiger", 
                  "beobachter", "derbund", "luzernerzeitung", "aargauerzeitung", "bernerzeitung", 
                  "bazonline", "politnetz", "parlament", "20min", "srf", "tageswoche")
search_terms <- sort(as.character(search_terms))

for(i in search_terms){
dfall$paper <- ifelse(grepl(paste0(".",i), dfall$url), paste(i), dfall$paper)
}

dfsub <- dfall %>% filter(!is.na(paper))

#SE Function:
se <- function(x) sqrt(var(x)/length(x))

dfsub <- filter(dfsub, language %in% c("german", "french", "italian"))
dfplotpaperfull <- dfsub %>% group_by(poll, paper) %>% summarize(Avg = mean(sentiment, na.rm = T), SE = se(sentiment), SUM  = n())



dfplotnobillag <- dfplotpaperfull %>% filter(poll == "no-billag")

pap1 <-ggplot(dfplotnobillag) +
  geom_point(aes(x=paper, y=Avg), size = 2, shape = 21,  position=position_dodge(width = 0.75)) +
  geom_errorbar(aes(x = paper, ymin = Avg-SE, ymax = Avg+SE), width = .2,  position=position_dodge(width = 0.75)) +
  geom_hline(yintercept = 0, color = "dark grey", linetype = "dashed") +
  geom_text(aes(y = Avg-(SE+.17), x = paper, label = paste0("n = ",SUM)), position=position_dodge(width = 0.75), vjust = .5, hjust = 0.5) +
  geom_text(aes(y = .5, x = 0.75, label = paste0("Total number of Texts: ", sum(SUM)))) +
  labs(title = "Variation of Sentiment by papers \nfor the no-billag initiative:", y = "Average Sentiment", x ="Source of Text") +
  scale_y_continuous(limits = c(-2,1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_colour_startrek() +
  theme(legend.position="bottom",legend.direction="horizontal") +
  coord_flip() +
  guides(color=FALSE) 

ggsave("Sent_Diff_by_Paper_billag.png", pap1, device = "png", width = 6, height = 9, dpi = 300)
ggsave("Sent_Diff_by_Paper_billag.pdf", pap1, device = "pdf", width = 6, height = 9, dpi = 300)


dfplotAlter <- dfplotpaperfull %>% filter(poll == "Altersvorsorge 2020")

pap2 <- ggplot(dfplotAlter) +
  geom_point(aes(x=paper, y=Avg), size = 2, shape = 21,  position=position_dodge(width = 0.75)) +
  geom_errorbar(aes(x = paper, ymin = Avg-SE, ymax = Avg+SE), width = .2,  position=position_dodge(width = 0.75)) +
  geom_hline(yintercept = 0, color = "dark grey", linetype = "dashed") +
  geom_text(aes(y = Avg-(SE+.17), x = paper, label = paste0("n = ",SUM)), position=position_dodge(width = 0.75), vjust = .5, hjust = 0.5) +
  geom_text(aes(y = .75, x = 0.75, label = paste0("Total number of Texts: ", sum(SUM)))) +
  labs(title = "Variation of Sentiment by papers \nfor the Altersvorsorge 2020 referenda:", y = "Average Sentiment", x ="Source of Text") +
  scale_y_continuous(limits = c(-1.8,1.5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_colour_startrek() +
  theme(legend.position="bottom",legend.direction="horizontal") +
  coord_flip() +
  guides(color=FALSE) 

ggsave("Sent_Diff_by_Paper_Alter.png", pap2, device = "png", width = 6, height = 9, dpi = 300)
ggsave("Sent_Diff_by_Paper_Alter.pdf", pap2, device = "pdf", width = 6, height = 9, dpi = 300)

dfplotVollgeld <- dfplotpaperfull %>% filter(poll == "Vollgeld")

pap3 <- ggplot(dfplotVollgeld) +
  geom_point(aes(x=paper, y=Avg), size = 2, shape = 21,  position=position_dodge(width = 0.75)) +
  geom_errorbar(aes(x = paper, ymin = Avg-SE, ymax = Avg+SE), width = .2,  position=position_dodge(width = 0.75)) +
  geom_hline(yintercept = 0, color = "dark grey", linetype = "dashed") +
  geom_text(aes(y = Avg-(SE+.17), x = paper, label = paste0("n = ",SUM)), position=position_dodge(width = 0.75), vjust = .5, hjust = 0.5) +
  geom_text(aes(y = .75, x = 0.75, label = paste0("Total number of Texts: ", sum(SUM)))) +
  labs(title = "Variation of Sentiment by papers \nfor the Vollgeld initiative:", y = "Average Sentiment", x ="Source of Text") +
  scale_y_continuous(limits = c(-2,1.5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_colour_startrek() +
  theme(legend.position="bottom",legend.direction="horizontal") +
  coord_flip() +
  guides(color=FALSE) 

ggsave("Sent_Diff_by_Paper_Geld.png", pap3, device = "png", width = 6, height = 9, dpi = 300)
ggsave("Sent_Diff_by_Paper_Geld.pdf", pap3, device = "pdf", width = 6, height = 9, dpi = 300)

dfplotSpiel <- dfplotpaperfull %>% filter(poll == "Geldspielgesetz")

pap4 <- ggplot(dfplotSpiel) +
  geom_point(aes(x=paper, y=Avg), size = 2, shape = 21,  position=position_dodge(width = 0.75)) +
  geom_errorbar(aes(x = paper, ymin = Avg-SE, ymax = Avg+SE), width = .2,  position=position_dodge(width = 0.75)) +
  geom_hline(yintercept = 0, color = "dark grey", linetype = "dashed") +
  geom_text(aes(y = Avg-(SE+.17), x = paper, label = paste0("n = ",SUM)), position=position_dodge(width = 0.75), vjust = .5, hjust = 0.5) +
  geom_text(aes(y = .75, x = 0.75, label = paste0("Total number of Texts: ", sum(SUM)))) +
  labs(title = "Variation of Sentiment by papers \nfor the Geldspielgesetz referenda:", y = "Average Sentiment", x ="Source of Text") +
  scale_y_continuous(limits = c(-2,1.5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_colour_startrek() +
  theme(legend.position="bottom",legend.direction="horizontal") +
  coord_flip() +
  guides(color=FALSE) 

ggsave("Sent_Diff_by_Paper_Spiel.png", pap4, device = "png", width = 6, height = 9, dpi = 300)
ggsave("Sent_Diff_by_Paper_Spiel.pdf", pap4, device = "pdf", width = 6, height = 9, dpi = 300)

dfplotpaperallpoll <- dfsub %>% group_by(paper) %>% summarize(Avg = mean(sentiment, na.rm = T), SE = se(sentiment), SUM  = n())

pap5 <- ggplot(dfplotpaperallpoll) +
    geom_point(aes(x=paper, y=Avg), size = 2, shape = 21,  position=position_dodge(width = 0.75)) +
    geom_errorbar(aes(x = paper, ymin = Avg-SE, ymax = Avg+SE), width = .2,  position=position_dodge(width = 0.75)) +
    geom_hline(yintercept = 0, color = "dark grey", linetype = "dashed") +
    geom_text(aes(y = Avg-(SE+.17), x = paper, label = paste0("n = ",SUM)), position=position_dodge(width = 0.75), vjust = .5, hjust = 0.5) +
    geom_text(aes(y = .5, x = 0.75, label = paste0("Total number of Texts: ", sum(SUM)))) +
    labs(title = "Variation of Sentiment by papers:", y = "Average Sentiment", x ="Source of Text") +
    scale_y_continuous(limits = c(-0.85,.85)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    scale_colour_startrek() +
    theme(legend.position="bottom",legend.direction="horizontal") +
    coord_flip() +
    guides(color=FALSE) 

ggsave("Sent_Diff_by_Papers.png", pap5, device = "png", width = 6, height = 9, dpi = 300)
ggsave("Sent_Diff_by_Papers.pdf", pap5, device = "pdf", width = 6, height = 9, dpi = 300)
##################################################################################################