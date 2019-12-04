library(rvest)
library(jsonlite)
library(data.table)
library(tidyverse)
library(pbapply)
library(dplyr)
library(purrr)

# The code below scrapes the complete NASDAQ data of 15,250 companies.
# It displays the company's market share and the NASDAQ's analysts advice on stock trading, 
# the News Sentiment, Media Buzz, HedgeFund Sentiment and Investor Sentiment

setwd("D:/Web Scraping/team assignment")
all_urls <- list()

for (i in 1:304) {
    one_url <- paste0("https://www.nasdaq.com/api/v1/screener?page=", i, "&pageSize=20")
    all_urls <- rbind(all_urls, one_url)
}


one_page_data <- function(x) {
   
    jdata <- fromJSON(x, flatten = TRUE)
    
    jdata$data$priceChartSevenDay <- NULL
    jdata$data$articles <- NULL
    jdata$count <- NULL
    
    Ticker <- jdata$data$ticker
    Company <- jdata$data$company
    Market_capital <- jdata$data$marketCap
    Market_share <- jdata$data$marketCapGroup
    Sector <- jdata$data$sectorName
    Analyst_advice <- jdata$data$analystConsensusLabel
    News_Sentiment <- jdata$data$newsSentimentData.signal
    News_Score <- jdata$data$newsSentimentData.score
    Media_Buzz <- jdata$data$mediaBuzzData.signal
    Media_Score <- jdata$data$mediaBuzzData.score
    HedgeFund_Sentiment <- jdata$data$hedgeFundSentimentData.signal
    HedgeFund_Score <- jdata$data$hedgeFundSentimentData.score
    Investor_Sentiment <- jdata$data$investorSentimentData.signal
    Investor_Score <- jdata$data$investorSentimentData.score
    
    one_nasdaq <- data.frame(Ticker, Company, Market_capital, Market_share, Sector, Analyst_advice, News_Sentiment, News_Score, Media_Buzz,
                             Media_Score, HedgeFund_Sentiment, HedgeFund_Score, Investor_Sentiment, Investor_Score)
    
    return(one_nasdaq)
}


nasdaq_data <- rbindlist(pblapply(all_urls, one_page_data))

saveRDS(nasdaq_data, file = "nasdaqdata.rds")

write.csv(nasdaq_data, file = "nasdaq.csv")

nrow(nasdaq_data)

# Analysis of the scraped data for NASDAQ:

view(nasdaq_data)    

# 1. Total number of Companies in the sector

companies_sector <- na.omit(nasdaq_data) %>% 
    group_by(Sector) %>%
    summarise('No_of_Companies' = n()) 
    
view(companies_sector)

# 2. Number of companies in each sector:  

ggplot(data = na.omit(nasdaq_data)) +
    geom_bar(mapping = aes(x = Sector, fill = Sector))

# 3. Sectors with respect to their market share: 

ggplot(data = na.omit(nasdaq_data)) +
    geom_bar(mapping = aes(x = Sector, fill = Market_share))

# 4. Summary statistics for investor's sentiments
ggplot(data = na.omit(nasdaq_data)) +
    stat_summary(
    mapping = aes(x = Investor_Sentiment, y = Investor_Score),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

# 5. sentiment analysis for Companies:   

Company_sentiments <- na.omit(nasdaq_data) %>% 
    mutate(mean_sentiments= rowMeans(data.frame(News_Score, Media_Score, HedgeFund_Score, Investor_Score))) 
    

Sentiments <- Company_sentiments$mean_sentiments

Company_sentiments$sentiment_bins <- cut(as.numeric(Sentiments), 5, 
                        labels = c("Strong_sell", "Moderate_sell", "Hold", "Moderate_buy", "strong_buy"))


Companies <- Company_sentiments %>% 
    group_by(Sector, Company, sentiment_bins) %>% 
    select(Company, sentiment_bins, mean_sentiments)

view(Companies)

# 6. Sentiment analysis by sector

ggplot(data = Companies) +
    geom_bar(mapping = aes(x = Sector, fill = sentiment_bins))

# 7. dot plot for mean sentiments and market capital:

Companies_2 <- Company_sentiments %>% 
    mutate(Market_cap_per_billion = Market_capital/1000000000)
view(Companies_2)

ggplot(data = Companies_2) +
    geom_point(mapping = aes(x = mean_sentiments, y = Market_cap_per_billion))

               