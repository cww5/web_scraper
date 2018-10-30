library(rvest)
movie <- read_html("http://www.imdb.com/title/tt1431045")
cred_sum <- movie %>% html_nodes(".credit_summary_item") %>% html_text()
sum <- movie %>% html_node(".summary_text") %>% html_text()

journal <- read_html("http://genomebiology.biomedcentral.com/articles")
articles <- journal %>% html_nodes(".c-teaser__title") %>% html_text()

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
rm(list = ls())
