library(tidyverse)
library(rvest)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)

rm(list = ls())
set.seed(1234)
options(scipen=999)

setwd("/Users/adrianharris/Desktop/Messy-Data-Project")

# Attack 
urls <- c()

pagesRange <- 1:57797
maxPage <- 264 

for(i in 1:57797){
  urls[i] <- paste0("https://secure.runescape.com/m=hiscore/ranking?category_type=0&table=1&date=1648423839978&time_filter=0&page=",i)
}

spots <- round(max(pagesRange)/maxPage)

n <- length(pagesRange)
idx <- sample(rep(1:spots, each = ceiling(n /spots))[1:n], replace = F)
urlList <- split(urls, idx)

              
dataGrabber <- function(url){
  
  rank <- read_html(url) %>% 
    html_nodes(xpath = "//td[@class='col1 align']") %>%
    html_text(trim=TRUE)
  
  name <- read_html(url) %>% 
    html_nodes(xpath = "//td[@class='col2']") %>%
    html_text(trim=TRUE)
  
  level <- read_html(url) %>% 
    html_nodes(xpath = "//td[@class='col3 align']") %>%
    html_text(trim=TRUE)
  
  xp <- read_html(url) %>% 
    html_nodes(xpath = "//td[@class='col4 align']") %>%
    html_text(trim=TRUE)
  
  xp <- as.numeric(gsub(",","",xp))
  
  df <- tibble('Player' = name,
               'Level' = level,
               'Xp' = xp)
  
  return(df)
}

# Randomly select pages from the full length of pages 
# then sample with out replacement 
# keeo that vector of values so you don't use them agian 
# keeping doing that every day till the dataset is built

dataList <- vector("list",length(1:maxPage))

# Page 
for(i in 1:length(dataList)){
  dataList[[i]] <- dataGrabber(urlList[[11]][i])
}

i


df <- do.call(rbind, dataList)

View(df)
# Remember Change the Page number 
write.csv(df,"Data/Atk/RunescapeAtkLeaderboard11.csv", row.names = FALSE)


