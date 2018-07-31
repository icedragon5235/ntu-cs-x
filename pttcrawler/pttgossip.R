library(XML)
library(xml2)
library(rvest)
library(httr)
library(dplyr)
library(RCurl)
from <- 39507
to   <- 39508
prefix = "https://www.ptt.cc/bbs/Gossiping/index"

data <- list()
for( id in c(from:to) )
{
  url  <- paste0( prefix, as.character(id), ".html" )
  html <-htmlParse( GET(url,set_cookies(over18 = 1)) )
  url.list <- xpathSApply( html, "//div[@class='title']/a[@href]", xmlAttrs )
  data <- rbind( data, as.matrix(paste('https://www.ptt.cc', url.list, sep='')) )
}
data <- unlist(data)
getdoc <- function(url)
{
  get<-GET(url,config=set_cookies("over18"="1"))
  get_txt<-content(get,"text")
  html<-htmlParse(get_txt)
  doc  <- xpathSApply( html, "//div[@id='main-content']", xmlValue )
  time <- xpathSApply( html, "//*[@id='main-content']/div[4]/span[2]", xmlValue )
  temp <- gsub( "  ", " 0", unlist(time) )
  part <- strsplit( temp, split=" ", fixed=T )
  date <- paste(part[[1]][2], part[[1]][3], part[[1]][5], sep="-")
  timestamp <- part[[1]][4]
  timestamp <- strsplit( timestamp, split=":", fixed=T )
  tt <- paste0(timestamp[[1]][1],timestamp[[1]][2],timestamp[[1]][3])
  name <- paste0('C:/Users/user/Desktop/codingsite/ntu-cs-x/pttcrawler/gossip/',date,tt, ".txt")
  write(doc, name,append=TRUE)
}

sapply(data, getdoc)
file.exists('./pttcrawler/')

path <- "C:/Users/user/Desktop/codingsite/ntu-cs-x/pttcrawler/gossip/"
file.names <- list.files(path, pattern = ".txt")
files <- paste0(path, file.names)
files
contents <- lapply(files, readLines)

is.push <- function(line){
  if(startsWith(line, "推") || startsWith(line, "噓") || startsWith(line, "→ ")){
    return (T)
  }else{
    return (F)
  }
}

get.line.datetime <- function(line){
  len <- nchar(line)
  datetime <- substring(line, len - 10, len)
  return(datetime)
}

get.content.datetime <- function(content){
  push.position <- sapply(content, is.push)
  push <- content[push.position]
  datetime <- sapply(push, get.line.datetime)
  names(datetime) <- NULL
  return(datetime)
}

lapply(contents, get.content.datetime)

library(lubridate)
catchwant <- function(line,publishtime){
  len <- nchar(line)
  datetime <- substring(line, len - 10, len)
  as.time(datetime)
  as.time(publishtime)
  
}
