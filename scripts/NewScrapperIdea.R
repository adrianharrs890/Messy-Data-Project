
library(tidyverse)
library(rvest)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)

rm(list = ls())
set.seed(1234)
options(scipen=999)

setwd("/Users/adrianharris/Desktop/MDML-Project")

# Getting players 
dataList <- vector("list", 10)

for(i in 1:10){
  dataList[[i]] <- read_csv(paste0("Data/Atk/RunescapeAtkLeaderboard",i, ".csv"))
}

atk <- do.call(rbind, dataList)

atk$Player <- gsub(pattern=" ",replacement="+",x=atk$Player)


Players <- unique(atk$Player)


splitSize <- round(length(Players)/2)

playerOne <- Players[1:splitSize]
playerTwo <- Players[splitSize:length(Players)]

urls <- c()
for(i in 1:max(length(playerOne),length(playerTwo))){
  urls[i] <- paste0("https://secure.runescape.com/m=hiscore/a=13/compare?user1=",playerOne[i],"&user2=",playerTwo[i])
}


pagesRange <- 1:50000
maxPage <- 250
spots <- round(max(pagesRange)/maxPage)

n <- length(pagesRange)
idx <- sample(rep(1:spots, each = ceiling(n /spots))[1:n], replace = F)
urlList <- split(urls, idx)




# Write the double for loop 
# New Code


# OLD Code
PlayersData <- vector("list", length(Players))

# 14249 last player
for(i in 14249:length(Players)){
  tmp <- read_html(urls[i])  %>% 
    html_table(fill = TRUE)
  
  test <- as.data.frame(tmp[[1]])
  test$Player = as.character(playerOne[i])
  test$Skill <- c("Overall", "Atk", "Def", "Str", 
                  "Constiution", "Range", "Prayer",
                  "Magic", "Cooking", "WoodCutting", 
                  "Fletching", "Fishing", "FireMaking", 
                  "Crafting", "Smithing", "Mining", "Herb",
                  "Agile", "Theiving","Slayer", "Farming",
                  "RuneCrafting", "Hunting", "Construction",
                  "Summoning", "Dungeoneering", "Div", 
                  "Inven" ,"Arc")
  
  testtwo <- as.data.frame(tmp[[2]])
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

df <- do.call(rbind, PlayersData)

nrow(df)

# Last Name
tail(df)

# Last I
# 14249
i
#length(playerTwo)
# 28948

write.csv(df,"NewData15.csv", row.names = FALSE)





