rm(list = ls())
library(rvest)
library(stringr)
library(data.table)
library(tidyverse)
#this creates a directory for the articles to go based on curwd (be careful in testing)
articles_dir <- paste(getwd(), "/articles/",sep="")
dir.create(file.path(articles_dir),showWarnings = FALSE)

#base url for each page of articles
#url <- "https://genomebiology.biomedcentral.com/articles?searchType=journalSearch&sort=PubDate&page=1"

#data frame to contain all final content 
all_content <- data.frame(doi=c(),title=c(),authors=c(),affiliates=c(),
                          corresp_author=c(),emails=c(),pub_date=c(),
                          abstract=c(),keywords=c(),full_text=c())

write_article_html <- function(str_doi, html_page){
  jtext <- as.character(html_page)
  conn <- file(paste(articles_dir, str_doi, ".html", sep=""))
  writeLines(jtext, conn)
  close(conn)
}
replace_empties <- function(elems){
  #Replaces empty elements in the list of article elements with NA
  for (i in seq_along(elems)){
    if(elems[[i]] == ""){
      #print("YO")
      elems[[i]] <- NA
    }
  }
  elems
}
#iterate through the pages and get the url for the corresponding page number
#page_number <- 0
num_pages <- c(125:126)
all_url <- "https://genomebiology.biomedcentral.com/articles?searchType=journalSearch&sort=PubDate&page=%d"
for(i in num_pages){
  #page_number <- page_number + 1
  cur_art_number <- 0
  url <- sprintf(all_url, i)

  #get the articles for the current page
  journal <- read_html(url)
  articles <- str_squish(journal %>% html_nodes(".c-teaser__title") %>% html_text())

  #get the information from the articles in the page 
  arts <- list() #a list of lists for the articles per page
  s <- html_session(url)
  for (idx in seq_along(articles[1:length(articles)])){ #change to seq(1:length(articles))
    #cur_art_number <- cur_art_number + 1
    art_elems <- list() #this will be a list of each element needed from the current article
    art <- articles[idx]
    err_flag <- tryCatch(
      {
        page <- s %>% follow_link(art) %>% read_html()
      }, error=function(e) FALSE)
    if(length(err_flag) == 1){
      next
    }
    print(paste("Page: ",i," >Article Num: ",idx,sep=""))
    doi_base <- str_squish(page %>% html_nodes(".u-text-inherit") %>% html_text())[1]
    doi_url <- paste("https://genomebiology.biomedcentral.com/articles/", str_replace(doi_base, "(https://doi.org/)", ""), sep="")
    doi_str <- str_replace_all(doi_base, "(https://doi.org/)|/", "")
    art_elems[["doi"]] <- paste(doi_str,collapse=";")
    write_article_html(doi_str, page)
    art_elems[["title"]] <- paste(str_squish(page %>% html_nodes(".ArticleTitle") %>% html_text()),collapse=";")
    art_elems[["authors"]] <- paste(str_replace_all(str_squish(page %>% html_nodes(".AuthorName") %>% html_text()), "[0-9]|Email author", ""), collapse = ";")
    
    c_auths <- page %>% html_nodes(xpath="//*[@class='EmailAuthor']/preceding-sibling::span") %>% html_text()
    art_elems[["cores_auth"]] <- paste(c_auths, collapse=";")
    
    dt <- data.table(
      name = page %>% 
        html_nodes('meta') %>% 
        html_attr('name')
      ,content = page %>% 
        html_nodes('meta') %>% 
        html_attr('content')
    ) %>% 
      # extract both name and affiliatation, because make show they are matched.
      filter(name %in% c('citation_author_institution')) %>% 
      select(content)
  
    art_elems[["aut_affs"]] <- paste(dt$content, collapse=";")
    emails <- page %>% html_nodes('.EmailAuthor') %>% html_attr('href')
    art_elems[["email"]] <- paste(emails,collapse=";")
     
    pub_str <- str_squish(page %>% html_nodes(".u-reset-list") %>% html_text())
    pub_pos <- pub_str[grep("Published", pub_str)]
    art_elems[["pub_date"]] <- paste(regmatches(pub_pos,  regexpr("Published: [0-9][0-9]? [a-zA-Z]+ [0-9]+", pub_pos)), collapse=";")
    art_elems[["abs"]] <- paste(str_squish(page %>% html_nodes("#Abs1") %>% html_text()), collapse=";")
    art_elems[["keywords"]] <- paste(str_squish(page %>% html_nodes(".c-keywords") %>% html_text()), collapse=";")
    art_elems[["full_text"]] <- paste(str_squish(page %>% html_nodes("article") %>% html_text()), collapse = ";")
    art_elems <- replace_empties(art_elems)
    arts[[idx]] <- art_elems
  }

  #length(arts)
  #length(arts[[1]])
  for (i in seq_along(1:length(arts))){
    cur_art <- arts[[i]]
    new_row <- data.frame("DOI"=cur_art$doi,"Title"=cur_art$title,"Authors"=cur_art$authors,"Author Affiliations"=cur_art$aut_affs,
                          "Corresponding Author"=cur_art$cores_auth,"Corresponding Author\'s Email"=cur_art$email, 
                          "Publication Date"=cur_art$pub_date, "Abstract"=cur_art$abs,"Keywords"=cur_art$keywords,
                          "Full Text (Textual format)"=cur_art$full_text, check.names=FALSE)
    all_content <- rbind(all_content, new_row)
  }
  #print(url)
}
nrow(all_content)
ncol(all_content)
colnames(all_content)

write.table(all_content, file = "genomebiology.txt", sep = ",", row.names = FALSE, col.names = colnames(all_content),qmethod = "double",na="NA")

#rm(list = ls())
#library(rvest)
#library(data.table)
#library(tidyverse)
#xml <- read_html('https://genomebiology.biomedcentral.com/articles/10.1186/s13059-018-1535-9')

#xml %>% html_nodes('.EmailAuthor') %>% html_attr('href')
#> [1] "mailto:liuj@cs.uky.edu"
# get email address

#xml %>% html_nodes('.AuthorName') %>% html_text
#> [1] "Ye<U+00A0>Yu"  "Jinpeng<U+00A0>Liu" "Xinan<U+00A0>Liu" "Yi<U+00A0>Zhang"
#> [5] "Eamonn<U+00A0>Magner" "Erik<U+00A0>Lehnert" "Chen<U+00A0>Qian" "Jinze<U+00A0>Liu"
# get name

#dt <- data.table(
#  name = xml %>% 
#    html_nodes('meta') %>% 
#    html_attr('name')
#  ,content = xml %>% 
#   html_nodes('meta') %>% 
#    html_attr('content')
#) %>% 
  # extract both name and affiliatation, because make show they are matched.
#  filter(name %in% c('citation_author_institution')) %>% 
#  select(content)
#regmatches(var, gregexpr("([a-zA-Z]|[^\\x00-\\x7E])+( ([A-Z]|[^\\x00-\\x7E])\\.)? ([a-zA-Z]|[^\\x00-\\x7E])+\\^?[0-9]*(, [0-9])*Email author", var))
