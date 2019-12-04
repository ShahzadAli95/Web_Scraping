library(rvest)
library(data.table)
library(tidyverse)

# Below is a code that uses lapply and rbindlist to scrape news
# simply choose a topic from the list below and write the number of pages you want to scrape, it is actually a set of two functions
# The economist_news_2() is responsible for getting the data per page and creating a data frame
# The get_news() runs the function on each page using lapply and then combines them into a single data frame

topics_to_choose_from <- c("united-states", "the-americas", "middle-east-and-africa", "asia", "china", "europe", "britain", "international",
                           "business", "debates", "open-future", "obituary", "books-and-arts", "finance-and-economics", "special-reports") 

economist_news_2 <- function(topic_urls) {
    
    web_data <- read_html(topic_urls)
    
    my_titles <- 
        web_data %>%
        html_nodes('.flytitle-and-title__title')%>%
        html_text()
    
    
    my_summary <- 
        web_data %>%
        html_nodes('.teaser__text')%>%
        html_text()
    
    
    my_news <- 
        web_data %>%
        html_nodes('.flytitle-and-title__flytitle')%>%
        html_text()
    
    return(data.frame('title'= my_titles, 'summary'= my_summary, 'news'= my_news))
    
}

get_news <- function(topic, pages_no) {
    
    topic_urls <- paste0("https://www.economist.com/", topic , "/", "?page=", seq(from=1, to=pages_no, by=1))
    
    my_res <- lapply(topic_urls, economist_news_2)
    
    res_df <- rbindlist(my_res)
    
    view(res_df)
    
}

# Examples: 
get_news("europe", 2)
get_news("britain", 4)
get_news("the-americas", 20)

###################################################
###################################################
###################################################

# Below is another function that does a very similar job but uses a for loop to switch the pages on the website

full_news <- data.frame()

economist_news <- function(topic, pages_no) {
    
    for (i in 1:pages_no) {
        
    topic_pages <- paste0("https://www.economist.com/", topic , "/", "?page=", i)
    
    web <- read_html(topic_pages)
    
    my_titles <- 
        web %>%
        html_nodes('.flytitle-and-title__title')%>%
        html_text()
    
    
    my_summary <- 
        web %>%
        html_nodes('.teaser__text')%>%
        html_text()
    
    
    my_news <- 
        web %>%
        html_nodes('.flytitle-and-title__flytitle')%>%
        html_text()
    my_titles
    
    news_data <- data.frame('title'= my_titles, 'summary'= my_summary, 'news'= my_news)
    full_news <- rbind.data.frame(full_news, news_data)
    
    }
 
    view(full_news)
}

# Examples: 
economist_news("the-americas", 10)
economist_news("china", 5)

