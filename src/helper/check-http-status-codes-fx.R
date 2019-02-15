library(R.utils)
library(httr)

check_status_code <- function(hits){
  
  if(is.list(hits)) hits <- unlist(hits)
  
  hits.bool <- pbsapply(hits, function(x){
    res <- tryCatch(
      {withTimeout({!http_error(x)}, timeout = 10)},
      TimeoutException = function(ex){NA}, 
      error = function(e){NA}
      ) 
  }) %>% 
    tibble(boolean=., url=names(.))
  return(hits.bool)
}
