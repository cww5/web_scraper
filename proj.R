rm(list = ls())
library(rvest)
library(stringr)
url <- "http://genomebiology.biomedcentral.com/articles/"
journal <- read_html(url)
articles <- str_squish(journal %>% html_nodes(".c-teaser__title") %>% html_text())
articles
new <- list()
s <- html_session(url)
for (i in articles[1:15]){
  page <- s %>% follow_link(i) %>% read_html()
  new[[i]] <- page %>% html_nodes("#Abs1") %>% html_text()
}
new





movie <- read_html("http://www.imdb.com/title/tt1431045")
cred_sum <- movie %>% html_nodes(".credit_summary_item") %>% html_text()
sum <- movie %>% html_node(".summary_text") %>% html_text()


library(rvest)
url<- "http://www.imdb.com/title/tt0974015/"
cast <- str_squish(read_html(url) %>% html_nodes("td") %>% html_text())
cast <- cast[-c(1,seq(2,62,by=2),62:70)]
cast <- cast[-seq(2,30,by=2)]
length(cast) # 15
new <- list()
s <- html_session(url)
for (i in seq_along(cast)){
  page <- s %>% follow_link(i) %>% read_html()
  new[[i]] <- page %>% html_nodes("b a") %>% html_text()
}





library(rvest)
library(stringr)
library(data.table)
lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")
cast <- lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
cast

s <- html_session("http://www.imdb.com/title/tt1490017/")

cast_movies <- list()

for(i in cast[1:3]){
  actorpage <- s %>% follow_link(i) %>% read_html()
  cast_movies[[i]]$movies <-  actorpage %>% 
    html_nodes("b a") %>% html_text() %>% head(10)
  cast_movies[[i]]$years <- actorpage %>%
    html_nodes("#filmography .year_column") %>% html_text() %>% 
    head(10) %>% str_extract("[0-9]{4}")
  cast_movies[[i]]$name <- rep(i, length(cast_movies[[i]]$years))
}

cast_movies
as.data.frame(cast_movies[[1]])
rbindlist(cast_movies)


















library (data.table)
library(XML)
pages <- c("data-scientist-midwest-or-silicon-valley","statistician-and-r-developer-with-gmm-expertise")
urls <- rbindlist(lapply(pages,function(x){
  url<-paste("http://www.r-users.com/jobs/",x,"/",sep="")
  data.frame(url)
}),fill=TRUE)

joblocations <- rbindlist(apply(urls, 1, function(url){
  doc1 <- htmlParse(url)
  locations <- getNodeSet(doc1, '//*[@id="mainContent"]/div[2]/o1/li/dl/dd[3]/span/text()')
  data.frame(sapply(locations, function(x) { xmlValue(x) }))
}), fill=TRUE)


library(RCurl)
library(XML)

doc.html = htmlTreeParse("http://genomebiology.biomedcentral.com/articles", useInternal = TRUE)

articles = "http://genomebiology.biomedcentral.com/articles"
html <- getURL(articles)

# parse html
doc = htmlParse(html, asText=TRUE)
Title = xpathSApply(doc, "//*[@id='artTitle']", xmlValue)
Authors = xpathSApply(doc, "//*[@id='floatAuthorList']", xmlValue)
## remove some special characters in the Authors to get clean text
Authors = gsub("\n", " ", Authors)
## Split the Authors by "   ,   " to get every author (See R regular expression for more information)
Authors = strsplit(Authors,'\\s*,\\s*')

PubDate = xpathSApply(doc, "//*[@id='artPubDate']", xmlValue)
## Get clean date
PubDate = gsub('Published:\\s*', '', PubDate)

Abstract = xpathSApply(doc, "//*[@class='abstract toc-section']/*[@title='Abstract']/../p", xmlValue)

