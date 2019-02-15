#################################################################
# Google links scraper
#################################################################
# How this function can be used is demonstrated @ ...
#################################################################
# Content
#################################################################
# Dependencies
# Search query URL generator
# Search query result scraper
#################################################################


#################################################################
# Dependencies
#################################################################
library(RCurl)
library(XML)

#################################################################
# Search query URL generator
#################################################################
# parameters: search terms, language, google domain, whether to use quotes, number of pages
get_search_url <- function(search.term, language = 'de',  domain = '.ch', quotes=TRUE, n.pages=1){
  # search term without quotes
  search.term <- gsub(' ', '%20', search.term) 
  # search term without quotes
  if(isTRUE(quotes)) search.term <- paste('%22', search.term, '%22', sep='') 
  # paste everything together
  google.url <- paste('http://www.google', domain, '/search?','hl=', language,'&q=',
                        search.term, sep='')
  # if more than one page, add respective suffix and return, else return
  if(n.pages>1){
    return(c(google.url, paste0(google.url, '&ei=q-W9W-2MBoTCwALs5aPwBg&start=', (1:(n.pages-1))*10, '&sa=N')))
  }else{
    return(google.url)
  }
}

#################################################################
# Search query result scraper
#################################################################
# parameters: google url, whether we want to return raw URLs, whether to drop recursive google URLs (e.g. picture or video recommendations)
get_google_hits <- function(google.url, raw=T, drop.recursives=F) {
  # curl page
  doc <- getURL(URLencode(google.url), .opts=curlOptions(followlocation=TRUE, cookiefile="nosuchfile"))
  # strip html
  html <- htmlTreeParse(doc, useInternalNodes = TRUE, error=function(...){})
  # get the respective nodes
  nodes <- getNodeSet(html, "//h3[@class='r']//a")
  # scrape the links
  raw.refs <- sapply(nodes, function(x) x <- xmlAttrs(x)[["href"]])
  # drop recommendation links (picture or video recommendations [recursive searches])
  if(isTRUE(drop.recursives)) raw.refs <- raw.refs[!grepl('\\/search\\?q\\=', raw.refs)]
  # determine URL fromatting
  if(isTRUE(raw)){
    return(raw.refs)
    }else{
    clean.refs <- gsub('(\\/url\\?q\\=)|(\\&sa.*)', '', raw.refs) %>% 
      sapply(., function(x) URLdecode(URLdecode(x)))
    return(clean.refs)
  }
}
