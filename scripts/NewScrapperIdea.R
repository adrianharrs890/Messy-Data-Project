
library(tidyverse)
library(rvest)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)

rm(list = ls())
set.seed(1234)
options(scipen=999)


# setwd("/Users/adrianharris/Desktop/MDML-Project")

setwd("/Users/adrianharris/Desktop/Messy-Data-Project")

# Getting players from the previous idea of 
# Pulling each skill

#**Note start scraping from 11 plus in the atk scraper
dataList <- vector("list", 15)

for(i in 1:15){
  dataList[[i]] <- read_csv(paste0("Data/Atk/RunescapeAtkLeaderboard",i, ".csv"))
}

atk <- do.call(rbind, dataList)

# Replacing the space with a "+" because thats how the compare page 
# takes players 
atk$Player <- gsub(pattern=" ",replacement="+",x=atk$Player)

# This will allow us to have about 50k players to pull there 
# full profile from the compare page 
Players <- unique(atk$Player)


# Splitting the players into two equal vectors to form urls with 
splitSize <- round(length(Players)/2)
playerOne <- Players[1:splitSize]
playerTwo <- Players[splitSize:length(Players)]

# Generating Urls 
urls <- c()
for(i in 1:max(length(playerOne),length(playerTwo))){
  urls[i] <- paste0("https://secure.runescape.com/m=hiscore/a=13/compare?user1=",playerOne[i],"&user2=",playerTwo[i])
}

# New max length 
length(playerOne)

# Making a list to fill the webscraped data 
# it should be the  length of max of player one or player two 
# but this was a transfer from the old webscrapper
PlayersData <- vector("list", length(Players))

# 29067 last player
# The webscraper can only pull about 1k urls (so 2k players) at a time 
# on a given url for example the last i it stopped at was 15321

for(i in 29067:length(Players)){
  tmp <- read_html(urls[i])  %>% 
    html_table(fill = TRUE) # the player info is on a table in the page 
  
  test <- as.data.frame(tmp[[1]]) # Player one is on index 1 
  test$Player = as.character(playerOne[i])
  test$Skill <- c("Overall", "Atk", "Def", "Str", 
                  "Constiution", "Range", "Prayer",
                  "Magic", "Cooking", "WoodCutting", 
                  "Fletching", "Fishing", "FireMaking", 
                  "Crafting", "Smithing", "Mining", "Herb",
                  "Agile", "Theiving","Slayer", "Farming",
                  "RuneCrafting", "Hunting", "Construction",
                  "Summoning", "Dungeoneering", "Div", 
                  "Inven" ,"Arc") # Making the vector of the order of the skill 
  
  testtwo <- as.data.frame(tmp[[2]]) # Sam process as above 
  testtwo$Player = as.character(playerTwo[i])
  testtwo$Skill <- c("Overall", "Atk", "Def", "Str", 
                     "Constiution", "Range", "Prayer",
                     "Magic", "Cooking", "WoodCutting", 
                     "Fletching", "Fishing", "FireMaking", 
                     "Crafting", "Smithing", "Mining", "Herb",
                     "Agile", "Theiving","Slayer", "Farming",
                     "RuneCrafting", "Hunting", "Construction",
                     "Summoning", "Dungeoneering", "Div", 
                     "Inven" ,"Arc")
  
  PlayersData[[i]] <- rbind(test,testtwo)
  
}

# Joining all the datsets in the list to one
df <- do.call(rbind, PlayersData)

nrow(df)



# Last I so you know where to start from 
# 29067
i

#length(playerTwo) <- max number in the plater vec 
# 28948 # this was number before pulling more players

write.csv(df,"Data/ComparePage/NewData27.csv", row.names = FALSE)





