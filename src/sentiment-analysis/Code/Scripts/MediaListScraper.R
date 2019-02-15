################################################################################################
#Webscraper for swissdox.ch (Medienliste)
################################################################################################
library(RSelenium)
library(stringi)
library(dplyr)
library(RCurl)
library(httr)
library(XML)
library(readr)
################################################################################################
## Set Dir:
setwd("~/DigDemLab/Research Applications/Social Science One/Code/Output")
################################################################################################
#Start Selenium 
rD <- rsDriver(port=4445L, browser ="firefox")
remDr <- remoteDriver(port = 4445L, browserName = "firefox")
#Open Browser
remDr$open()
################################################################################################
# Get to Page "http://www.swissdox.ch/Swissdox2/index.jsp?groupId=4&contentId=4" 
url <- "http://www.swissdox.ch/Swissdox2/index.jsp?groupId=4&contentId=4"
remDr$navigate(url)
################################################################################
#Get the Table 
doc <- htmlParse(remDr$getPageSource()[[1]]) #Getting Sourcecode of the Page from remDr
doc <- readHTMLTable(doc)
doc <- as.data.frame(doc)
doc <- doc[-1,]
doc <- doc[,-4]

colnames(doc) <- c("Bezeichnung", "Erscheinungsweise", "Verfügbar seit")
doc$Bezeichnung <- as.character(doc$Bezeichnung)
doc$Erscheinungsweise <- as.character(doc$Erscheinungsweise)
doc$`Verfügbar seit` <- as.character(doc$`Verfügbar seit`)
################################################################################################
#Get the additional Information and add it to tha table 
################################################################################################
#allocate memory for a df of size of final data frame:
df <- data.frame()

#fill all the cells of data frame: 
for(i in 1:nrow(doc)){
  tryCatch({
    remDr$navigate(url)
    act_i_table <- i + 1
    moreinf <- remDr$findElement(using = "xpath", paste0("//*[@id='Content']/table/tbody/tr[",act_i_table,"]/th[4]/a"))
    Sys.sleep(1)
    moreinf$clickElement()
    Sys.sleep(1)
    
    code <- NA
    sprache <- NA
    medientyp <- NA
    gesamtauflage <- NA
    leserzahlen <- NA
    verleger <- NA
    internetadresse <- NA

    for(j in 1:35){
      tryCatch({
        act_j_more_info <- j
        type <- remDr$findElement(using = "xpath", paste0("//*[@id='showmediainfo']/div[",act_j_more_info,"]/div[1]")) 
        Sys.sleep(1)
        type <- type$getElementText() 
        
        # code, sprache, medientyp, leserzahlen, gesamtauflage, verleger, internetadresse
        row_of_i <- c(NA,NA,NA,NA,NA,NA,NA) 
        
        if(type == "Code"){
          code <- remDr$findElement(using = "xpath", paste0("//*[@id='showmediainfo']/div[",act_j_more_info,"]/div[2]"))
          code <- code$getElementText()
          code <- unlist(code)
        } else if (type == "Sprache"){
          sprache <- remDr$findElement(using = "xpath", paste0("//*[@id='showmediainfo']/div[",act_j_more_info,"]/div[2]"))
          sprache <- sprache$getElementText()
          sprache <- unlist(sprache)
        } else if (type == "Medientyp"){
          medientyp <- remDr$findElement(using = "xpath", paste0("//*[@id='showmediainfo']/div[",act_j_more_info,"]/div[2]"))
          medientyp <- medientyp$getElementText()
          medientyp <- unlist(medientyp)
        } else if (type == "Gesamtauflage"){
          gesamtauflage <- remDr$findElement(using = "xpath", paste0("//*[@id='showmediainfo']/div[",act_j_more_info,"]/div[2]"))
          gesamtauflage <- gesamtauflage$getElementText()
          gesamtauflage <- unlist(gesamtauflage)
        } else if (type == "Leserzahlen"){
          leserzahlen <- remDr$findElement(using = "xpath", paste0("//*[@id='showmediainfo']/div[",act_j_more_info,"]/div[2]"))
          leserzahlen <- leserzahlen$getElementText()
          leserzahlen <- unlist(leserzahlen)
        } else if (type == "Verleger"){
          verleger <- remDr$findElement(using = "xpath", paste0("//*[@id='showmediainfo']/div[",act_j_more_info,"]/div[2]"))
          verleger <- verleger$getElementText()
          verleger <- unlist(verleger)
        } else if (type == "Internetadresse"){
          internetadresse <- remDr$findElement(using = "xpath", paste0("//*[@id='showmediainfo']/div[",act_j_more_info,"]/div[3]"))
          internetadresse <- internetadresse$getElementText()
          internetadresse <- unlist(internetadresse)
        } else{
          cat("Things we do not need...")
          
        }
        #rm(code, sprache, medientyp, leserzahlen, gesamtauflage, verleger, internetadresse)
        
      }, error=function(e){cat("ERROR: ", conditionMessage(e), " Error Code on page: ", j, "\n")})
    }
    row_of_i[1]<- code
    row_of_i[2]<- sprache
    row_of_i[3]<- medientyp
    row_of_i[4]<- leserzahlen
    row_of_i[5]<- gesamtauflage
    row_of_i[6]<- verleger
    row_of_i[7]<- internetadresse
    
  
    row_of_i <- unlist(row_of_i)
    tmp <- cbind(doc[i,]$Bezeichnung, doc[i,]$Erscheinungsweise, doc[i,]$`Verfügbar seit`, row_of_i[1], row_of_i[2], 
                 row_of_i[3], row_of_i[4], row_of_i[5], row_of_i[6], row_of_i[7])
    df <- rbind(df,tmp)
    rm(tmp)
  
    Sys.sleep(1) 
 
  }, error=function(e){cat("ERROR: ", conditionMessage(e), " Error Code on page: ", i, "\n")}) 
}



head(df)

write_csv(df, "Medien_Liste.csv")
write_rds(df, "Medien_Liste.rds")
################################################################################################
#Close Selenium Windows and terminate the server
remDr$closeServer()
rD$server$stop()
