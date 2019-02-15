#################################################################
# Document burning fx
#################################################################
# How this function can be used is demonstrated @ ...
#################################################################
# Content
#################################################################
# Dependencies
# Word matching fx
# Document burner
## Default settings
## fx
### Text annotation
### Burning
#################################################################


#################################################################
# Dependencies
#################################################################
library(stringi)
library(dplyr)
library(tm)

#################################################################
# Word matching fx
#################################################################
buzz_matches <- function(char.list, buzzwords){
  # extract all ::buzzwords:: matches for an element
  matches <- stri_extract_all_regex(tolower(char.list), buzzwords)
  # if more than one buzzword, return ;-sep charstring
  matches <- sapply(matches, function(x){
    if(!any(is.na(x))){
      paste0(x, collapse = ';')
    }else{
        x
      }
  })
  
  return(matches)
}

#################################################################
# Document burner
#################################################################
## Default settings
#################################################################
default.buzzwords <- c('twitter',
                       'facebook', 
                       '(^impressum$)', 
                       'account', 
                       'aktivieren', 
                       'deaktivieren',
                       'abo', 
                       'adresse',
                       'strasse',
                       'e-mail',
                       'benutzerkonto',
                       '(anmelden (und|oder) registrieren)',
                       'angaben zu ihrer person',
                       'bleiben sie auf dem laufenden',
                       'keine news verpassen')

default.slicing.keywords <- c('impressum',
                              'bleiben sie auf dem laufenden',
                              'keine news verpassen')

#################################################################
## fx
#################################################################
# parameters:
clean_text <- function(
  txt,
  use.default=T, # use default buzzwords
  raw=F, # only returns annotated text, unburnt
  hard.filter=NULL, # vector with words that if they appear in line, line is dropped
  buzzwords=NULL, # character string with buzzwords
  slicing=T, # if slicing should be performed
  slicing.keywords=NULL, # deterministic slicing keywords [caution!]
  scnd.step.slicing=3, # last part of strings (here: last third) that will be used to compute the sum buzzwords
  scnd.step.threshold=4, # max number of buzzwords allowed in scnd.step.slicing before proceeding to slice the data
  min.words=3, # minimum number of words per single character string (used for filtering)
  min.avg.characters=3, # minimum number of avg.characters per word for each single character string (used for filtering)
  max.buzzwords=2, # max. number of buzzwords allowed per character string
  recover.fs=50
  ){
  
  #################################################################
  ### Text annotation
  #################################################################
  # coerce list objects to vector
  if(is.list(txt)){
    txt <- unlist(txt, recursive = F)
    }
  # empty vectors return NA and abort
  if(length(txt)%in%c(0,1)){
    return(NA)
    }
  # set default buzzwords and slicing keywords if wanted
  if(is.null(buzzwords)&use.default==T){ 
    buzzwords <- default.buzzwords
  }
  if(is.null(slicing.keywords)&use.default==T){
    slicing.keywords <- default.slicing.keywords
  }
  # create buzzword and slicing keyword regex
  buzzwords <- paste0(c(buzzwords, default.buzzwords), collapse = '|')
  slicing.keywords <- paste0(c(slicing.keywords, default.slicing.keywords), collapse = '|')
  # convert text data to ASCII
  txt <- iconv(enc2utf8(txt), 'UTF-8', 'ASCII')
  # split text into word vectors
  txt_splt <- sapply(txt, function(string) strsplit(string, ' '))
  # annotate text
  txt <- tibble(txt=txt, # original text
                n.words=lengths(txt_splt), # count words per text
                nchar.words.mean=sapply(txt_splt, function(x) mean(nchar(x), na.rm = T)), # mean number of characters per word per text
                buzz.words=buzz_matches(unlist(txt), buzzwords), # use word matching fx (see above)
                # count buzzwords
                n.buzzwords=lengths(
                  sapply(
                    strsplit(buzz.words, ';'), 
                    function(x){
                      if(any(is.na(x))){NULL}
                    }else{
                        x
                      }
                  )
                ),
                # add text id
                id=paste0('id_', 1:length(n.buzzwords))
  )
  if(isTRUE(raw)){
    return(txt) # return raw annotated text, unburnt
  }else{
    
    #################################################################
    ### Burning
    #################################################################
    # create new log object from :txt:
    txt_log <- txt
    #### sclicing
    # if there are ::sclicing.keywords:: in the MATCHED ::buzz.words:: (== txt$buzz.words)
    if(isTRUE(slicing)&length(grep(slicing.keywords, txt$buzz.words))>0){
      # if some ::buzz.words:: have been matched multiple times...
      if(any(table(txt$buzz.words, useNA = 'no')>1)){
        # ... we take the first instance for which at least one ::slicingkey.word:: matches the matched ::buzz.words::
        # == first scraped element of the DOM satisfying this condition
        txt <- txt[as.integer(seq_len(max(which(grepl(slicing.keywords, txt$buzz.words)))-1)),]
      }else{
        # if not we take last instance for which at least one  ::slicingkey.word:: matches the matched ::buzz.words::
        # == last scraped element of the DOM satisfying this condition
        txt <- txt[as.integer(seq_len(min(which(grepl(slicing.keywords, txt$buzz.words)))-1)),]
      }
    }
    # drop lines exceeding the respective limits set by the user|default
    txt <- filter(txt, !(n.words<min.words|nchar.words.mean<min.avg.characters|n.buzzwords>=max.buzzwords))
    # drop lines containing at least one ::hard.filter:: character
    if(!is.null(hard.filter)){
      txt <- filter(txt, !grepl(paste0(hard.filter, collapse = '|'), txt))
    }
    # if the number of buzzwords exceeds ::scnd.step.threshold:: ...
    if(isTRUE(slicing)&sum(txt$n.buzzwords[round(nrow(txt)-nrow(txt)/scnd.step.slicing, 0):nrow(txt)])>scnd.step.threshold){
      # subset the chunk of elements that would be sliced off
      temp <- txt[round(nrow(txt)-nrow(txt)/scnd.step.slicing, 0):nrow(txt),]
      # extract first :id: actually containing buzzwords 
      id <- temp$id[min(which(temp$n.buzzwords>0))]
      # filter all elements up to said id
      txt <- txt[as.integer(seq_len(min(which(txt$id==id))-1)),]
    }
    # if there are elements clearly containing too many character to be dropped without any check (== exceeding ::recover.fs::), recover them
    txt <- txt_log[(txt_log$id%in%txt$id)|(txt_log$n.words>=recover.fs),]
    
    return(txt)
  }
  
}
                           
