#===Chapter 3===

library(tidyverse)
library(rvest)
library(RSelenium)
#=see this link for documentation: http://rvest.tidyverse.org/
#=open a browser and link to the page=
grassroots.url <- "http://grassrootsvolunteering.org/volunteer_opportunities"
grassroots.session <- html_session(grassroots.url)
grassroots <- grassroots.session

#=find out the number of the most recent index page=
page.latest <- grassroots %>%
  html_nodes("a") %>% # extract all <a> elements
  html_attr("href") %>%  # extract the attributes `href`
  str_subset("\\?page=[0-9]{2,}") %>% # find the `href` with the index number except for 1 (index[0-9]{2,})
  str_extract("[0-9]+") %>% # extract the number
  tail(1)
page.latest


#retrive links
link <- str_c(grassroots.url, "?page=", page.latest)
links.article <- grassroots %>%
  jump_to(link) %>% # move session to the most recent page
  html_nodes(css="h2") %>%
  html_nodes("a") %>% # extract article <a>
  html_attr("href") %>% # extract article <a> `href` attributes
  str_c("http://grassrootsvolunteering.org",.)
links.article

#========
#jump_to() and follow_link()

#html_session("http://hadley.nz/") %>% follow_link(css="#code .text-center a")
#nevigate to the link by clicking the github botton

#html_session("http://hadley.nz/") %>% jump_to("http://github.com/hadley/")
#jump to another session
#========

#=====
#html_node is like [[ it always extracts exactly one element. 
#When given a list of nodes, html_node will always return a list of the same length, 
#the length of html_nodes might be longer or shorter.
#=====

#Time to scrape page info~~
temp.html <- grassroots %>%
  jump_to(links.article[1])

organization.name <- temp.html %>%
  html_nodes(css="h1") %>%
  html_text()

organization.location <- temp.html %>%
  html_nodes(css="h4") %>%
  html_text()

organization.description <- temp.html %>%
  html_nodes(css=".left_tab_column")%>%
  html_nodes("p")%>%
  html_text()%>%
  str_c(collapse = " ")


#combine into tibble~
article.table <- tibble(org=organization.name,
                        location = organization.location,
                        description = organization.description,
                        url = links.article[1])    
article.table 

#======================
# Exercise 1: write a function to Scrape more than one index page
#======================
ScrapArticle <- function(u){
  grassroots.url <- "http://grassrootsvolunteering.org/volunteer_opportunities"
  grassroots.session <- html_session(grassroots.url)
  grassroots <- grassroots.session
  temp.html <- grassroots %>%
    jump_to(u)
  
  organization.name <- temp.html %>%
    html_nodes(css="h1") %>%
    html_text()
  
  organization.location <- temp.html %>%
    html_nodes(css="h4") %>%
    html_text()
  
  organization.info <- temp.html %>%
    html_nodes(css=".left_tab_column")
  
  organization.description <- organization.info[1]%>%
    html_nodes("p")%>%
    html_text() %>%
    str_c(collapse = " ")
  
  organization.duration <- organization.info[2]%>%
    html_nodes("p")%>%
    head(1)%>%
    html_text()
  
  organization.link <- temp.html%>%
    html_nodes(css=".apply")%>%
    html_nodes("a")%>%
    html_attr("href")
  
  
  
  #combine into tibble~
  article.table <- tibble(org=organization.name,
                          location = organization.location,
                          description = organization.description,
                          duration = organization.duration,
                          url = organization.link)    
  article.table 
  return(article.table)
}

ScrapePage <- function(pageNumber){
  link <- str_c(grassroots.url, "?page=", pageNumber)
  links.article <- grassroots %>%
    jump_to(link) %>% # move session to the most recent page
    html_nodes(css="h2") %>%
    html_nodes("a") %>% # extract article <a>
    html_attr("href") %>% # extract article <a> `href` attributes
    str_c("http://grassrootsvolunteering.org",.)
 scraped <- links.article %>%
    map(ScrapArticle)%>%
    bind_rows()
  return(scraped)
}

ScrapeWeb <- function(pages){
  page.latest <- grassroots %>%
    html_nodes("a") %>% # extract all <a> elements
    html_attr("href") %>%  # extract the attributes `href`
    str_subset("\\?page=[0-9]{2,}") %>% # find the `href` with the index number except for 1 (index[0-9]{2,})
    str_extract("[0-9]+") %>% # extract the number
    tail(1) %>%
    as.numeric()
  page.latest
  start <- page.latest-pages+1
  test <- c(start:page.latest)
  test%>%
    map(ScrapePage) -> scraped
  article.table.all <- scraped%>%
    bind_rows()
  return(article.table.all)
}

ScrapeAll <- function(){
  page.latest <- grassroots %>%
    html_nodes("a") %>% # extract all <a> elements
    html_attr("href") %>%  # extract the attributes `href`
    str_subset("\\?page=[0-9]{2,}") %>% # find the `href` with the index number except for 1 (index[0-9]{2,})
    str_extract("[0-9]+") %>% # extract the number
    tail(1) %>%
    as.numeric()
  ans <- ScrapeWeb(page.latest)
  write_csv(ans, "/Users/dylanwu/Desktop/df.csv")
  return(ans)
}


test <- ScrapeWeb(3)
