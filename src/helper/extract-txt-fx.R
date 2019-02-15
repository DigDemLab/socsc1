library(XML)
library(RCurl)

extract_txt <- function(urls, merged=TRUE, add.queries=NULL, preproc.expr=NULL){
  if(is.list(urls)) url <- unlist(urls)
  
  sapply(urls, function(s.url){
    html <- tryCatch(withTimeout({getURL(s.url, followlocation = TRUE)},
                                 timeout = 10), 
                     TimeoutException = function(ex){NA},
                     error=function(e){return(NA)})
    if(is.na(html)) return(NA)
    
    doc <- tryCatch(withTimeout({htmlParse(html, asText=TRUE)},
                                timeout = 10), 
                    TimeoutException = function(ex){NA},
                    error = function(e){return(NA)})
    if(is.na(doc)) return(NA)
    queries <- c(title = "//title", text = "//p", add.queries)
    plain.text <- tryCatch(withTimeout({xpathSApply(doc, queries, xmlValue)},
                                timeout = 10), 
                    TimeoutException = function(ex){NA},
                    error = function(e){return(NA)})
    if(all(is.na(plain.text))) return(NA)
    plain.text <- gsub('(\\{.*\\}(\\.)?)|(^\\.$)', '', plain.text)
    if(!is.null(preproc.expr)) plain.text <- plain.text[!grepl(preproc.expr, plain.text)]
    if(isTRUE(merged)){
      return(paste(plain.text, collapse = "\n"))
      }else{
        plain.text <- as.list(as.vector(plain.text))
        return(plain.text)
      }
  })
}

