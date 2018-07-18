if(!require(bitops))install.packages("bitops",repos = "http://cran.us.r-project.org")
if(!require(httr))install.packages("httr",repos = "http://cran.us.r-project.org")
if(!require(RCurl))install.packages("RCurl",repos = "http://cran.us.r-project.org")
if(!require(XML))install.packages("XML",repos = "http://cran.us.r-project.org")
if(!require(NLP))install.packages("NLP",repos = "http://cran.us.r-project.org")
if(!require(tmcn))install.packages("tmcn",repos = "http://cran.us.r-project.org")
if(!require(jiebaRD))install.packages("jiebaRD",repos = "http://cran.us.r-project.org")
if(!require(jiebaR))install.packages("jiebaR",repos = "http://cran.us.r-project.org")
library(bitops)
library(httr)
library(RCurl)
library(XML)
library(tm)
library(NLP)
library(tmcn)
library(jiebaRD)
library(jiebaR)

from <- 16479 # 2018-07-17
to   <- 16494 # 2018-07-17
prefix = "https://www.ptt.cc/bbs/C_Chat/index"

data <- list()
for( id in c(from:to) )
{
  url  <- paste0( prefix, as.character(id), ".html" )
  html <- htmlParse( GET(url) )
  url.list <- xpathSApply( html, "//div[@class='title']/a[@href]", xmlAttrs )
  data <- rbind( data, as.matrix(paste('https://www.ptt.cc', url.list, sep='')) )
}
data <- unlist(data)


library(dplyr)
getdoc <- function(url)
{
  html <- htmlParse( getURL(url) )
  doc  <- xpathSApply( html, "//div[@id='main-content']", xmlValue )
  time <- xpathSApply( html, "//*[@id='main-content']/div[4]/span[2]", xmlValue )
  temp <- gsub( "  ", " 0", unlist(time) )
  part <- strsplit( temp, split=" ", fixed=T )
  #date <- paste(part[[1]][2], part[[1]][3], part[[1]][5], sep="-")
  #date <- paste(part[[1]][2], part[[1]][5], sep="_")
  #date <- paste(part[[1]][1], part[[1]][2], sep="_")
  timestamp <- part[[1]][4]
  timestamp <- strsplit( timestamp, split=":", fixed=T )
  hour <- timestamp[[1]][1]
  #print(hour)
  name <- paste0('./week3/', hour, ".txt")
  write(doc, name, append = TRUE)
}

sapply(data, getdoc)

d.corpus <- Corpus( DirSource("./DATA") )
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})

mixseg = worker()
jieba_tokenizer = function(d)
{
  unlist( segment(d[[1]], mixseg) )
}
seg = lapply(d.corpus, jieba_tokenizer)

count_token = function(d)
{
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)

n = length(seg)
TDM = tokens[[1]]
colNames <- names(seg)
colNames <- gsub(".txt", "", colNames)
for( id in c(2:n) )
{
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', colNames[1:id])
}
TDM[is.na(TDM)] <- 0
library(knitr)
kable(head(TDM))
kable(tail(TDM))