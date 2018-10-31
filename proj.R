rm(list = ls())
library(rvest)
library(stringr)
#url <- "http://genomebiology.biomedcentral.com/articles/"
url <- "https://genomebiology.biomedcentral.com/articles?searchType=journalSearch&sort=PubDate&page=1"

all_content <- data.frame(doi=c(),title=c(),authors=c(),author_affiliates=c(),
                          corresp_author=c(),corresp_author_em=c(),pub_date=c(),
                          abstract=c(),keywords=c(),full_text=c())
tom <- data.frame(doi="doi",title="Title",authors="Your mom",author_affiliates="Yourdad",
                  corresp_author="Your dog",corresp_author_em="dog@gmail.com",pub_date="3/6/2008",
                  abstract="nah",keywords="awesome",full_text="such text very wow")
all_content <- rbind(all_content, tom)
#all_content
#nrow(all_content)
#ncol(all_content)

#write_journal_html<- function(doi){
#}

num_pages <- c(1:126)
all_url <- "https://genomebiology.biomedcentral.com/articles?searchType=journalSearch&sort=PubDate&page=%d"
for(i in num_pages){
  url <- sprintf(all_url, 7)
}

journal <- read_html(url)
articles <- str_squish(journal %>% html_nodes(".c-teaser__title") %>% html_text())
length(articles)
arts <- list()
s <- html_session(url)
for (idx in seq_along(articles[1:2])){
  new <- list()
  art <- articles[idx]
  page <- s %>% follow_link(art) %>% read_html()
  doi_str <- str_replace_all(str_squish(page %>% html_nodes(".u-text-inherit") %>% html_text())[1], "(https://doi.org/)|/", "")
  new[["doi"]] <- doi_str
  jtext <- as.character(page)
  conn <- file(paste(getwd(), "/articles/", doi_str, ".txt", sep=""))
  writeLines(jtext, conn)
  close(conn)
  new[["title"]] <- str_squish(page %>% html_nodes(".ArticleTitle") %>% html_text())
  new[["authors"]] <- str_replace_all(str_squish(page %>% html_nodes(".AuthorNames") %>% html_text()), "[0-9]|Email author", "")
  #new[[aut_affs]] <- str_squish(page %>% html_nodes("") %>% html_text())
  #new[[corr_auth]] <- str_squish(page %>% html_nodes("") %>% html_text())
  #new[[corr_auth_email]] <- str_squish(page %>% html_nodes("") %>% html_text())
  pub_str <- str_squish(page %>% html_nodes(".u-reset-list") %>% html_text())
  pub_pos <- grep("Published", pub_str)
  new[["pub_date"]] <- pub_str[pub_pos]
  new[["abs"]] <- str_squish(page %>% html_nodes("#Abs1") %>% html_text())
  new[["keywords"]] <- str_squish(page %>% html_nodes(".c-keywords") %>% html_text())
  #Replaces empty elements with NA
  for (i in seq_along(new)){
    if(length(new[[i]]) == 0){
      new[[i]] <- NA
    }
  }
  new[["full_text"]] <- str_squish(page %>% html_nodes(".FulltextWrapper") %>% html_text())
  arts[[idx]] <- new 
}
