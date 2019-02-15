#################################################################
# Google links scraper
#################################################################
# How this function can be used is demonstrated @ ...
#################################################################
# Content
#################################################################
# Dependencies
# Snowballing fx
# Wrapper to cycle the snowballer [control fx]
#################################################################


#################################################################
# Dependencies
#################################################################
library(stringr)
library(XML)
library(RCurl)

#################################################################
# Snowballing fx
#################################################################
# parameters: 
## url:: url
## keywords:: keywords that have to be present in HTML in order for link to be extracted [regex-ready!!!]
## key.match:: where the keywords have to be present
### opt:: outer == URL
### opt:: inner == html-full text (outer html!)
snowballer <- function(url, keywords=NULL, key.match='outer'){
  # extract the truncated URL (without the prefix)
  trunk <- str_extract(url, '^https?://[^/]+')
  # parse html if possible
  html <- paste(suppressWarnings(tryCatch({readLines(url, skipNul = T)}, error=function(e){e})), collapse="\n")
  # if parsing didn't result in error, extract all a-elements (links)
  # creates vector with all links contained in url
  if(!suppressWarnings(grepl('Error', html))){
    matched <- str_match_all(html, "<a href=\"(.*?)\"")
    links <- matched[[1]][,2] %>% unique
    links <- sapply(links, function(x){
      if(grepl('^/', x)){return(paste0(trunk, x))}else{
        # drop recursive links (linking to in-site content)
        if(grepl('^#', x)|!grepl('(http?(s))|(\\.html)', x)){return(NA)}else{
          return(x)
        }
      }
    }) %>% 
      unname %>% 
      na.omit
    # look for keywords in link vector, if keywords exist, and filters accordingly
    # in links
    if(!is.null(keywords)&key.match=='outer') links <- links[grepl(keywords, tolower(links))]
    # full-text matching
    if(!is.null(keywords)&key.match=='inner'){
      # creates logical vector
      # TRUE if outer html (full text) contains keywords
      # FALSE if it doesn't
      # NA if URL is not active anymore
      log <- sapply(links, function(x){
        if(url.exists(x)){
          html <- getURL(x, followlocation = TRUE)
          doc = htmlParse(html, asText=TRUE)
          queries <- c(title = "//title", text = "//p")
          plain.text <- xpathSApply(doc, queries, xmlValue)
          return(any(grepl(keywords, tolower(plain.text))))
        }else{
          return(NA)
        }
      })
      links <- links[ifelse(is.na(log), FALSE, log)]
    }
    return(links)
  }else{
    return(NULL)
  }
}

#################################################################
# Wrapper to cycle the snowballer [control fx]
#################################################################
# parameters:
## start:: URL from which snowballer starts
## time.limit:: abort snowballing from ::start:: after x time elapsed (prevents endless cycling) [seconds!!!]
## keywords:: hands keywords over to snowballing fx (see above)
## key.match:: hands matching methd over to snowballing fx (see above)
cycler <- function(start, time.limit, keywords, key.match='outer'){
  # initiate url log object
  url_log <- NULL  
  # initiate url list with ::start:: as first element
  url_list <- start
  # start timing
  strt <- Sys.time()
  # while loop that will be aborted as son as ::time.limit:: elapsed OR
  # if there is no link in :url_list: that has not already been collected and thus is also present in :url_log: 
  while(any(!url_list%in%url_log)){
    # add NEW links that will be scraped to url_log
    url_log <- c(url_list[!url_list%in%url_log], url_log)
    # scrape the NEW links for links == apply snowballer over NEW links
    # creates new version of :url_list: where every link vector created for NEW links respectively is appended as new element to :url_list:
    url_list <- tryCatch(lapply(url_list, function(x){
      res <- snowballer(x, keywords=keywords)
      stp <- Sys.time()
      if(difftime(stp, strt, units = c('secs'))>time.limit){
        break
      }else{
          return(res)
        }
      }) %>% 
      unlist %>% 
      c(url_list, .) %>% 
      unique, 
      error=function(e){
        return(unique(c(url_list, url_log)))
        }
      )
    stp <- Sys.time()
    if(difftime(stp, strt, units = c('secs'))>time.limit) break
  }
  
  return(url_list)
}
