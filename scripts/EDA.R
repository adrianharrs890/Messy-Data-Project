library(tidyverse)
library(rvest)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(janitor)

rm(list = ls())
set.seed(1234)
options(scipen=999)

setwd("/Users/adrianharris/Desktop/Messy-Data-Project")

dataList <- vector("list", 16)

for(i in 1:16){
  dataList[[i]] <- read.csv(paste0('Data/ComparePage/NewData',i,".csv"))
}

df <- do.call(rbind, dataList)
head(df, 4)

nrow(df)
# Checking for duplicates 

#nrow(df)
#nrow(distinct(df))
#length(unique(df$Player))
#17755/2


#df <- df %>%
  #distinct()

df <- df %>%
  mutate(Rank = as.character(Rank),
         Total.XP = as.character(Total.XP),
         Level = as.character(Level))

df <- df %>%
  mutate(Rank = as.numeric(gsub(",","", Rank)),
         Level = as.numeric(gsub(",","", Level)),
         Total.XP = as.numeric(gsub(",","", Total.XP)))

str(df)

# Distribution of Levels
# Some Skill go to 120 
# Most go to 99
df %>%
  filter(Level < 120) %>%
  ggplot(.) + 
  aes(Level) + 
  geom_histogram()

# Distribution of Levels by Skill
df %>%
  filter(Level < 120) %>%
  ggplot(.) + 
  aes(Level) + 
  geom_histogram() + 
  facet_wrap(~Skill)

df %>%
  filter(Level > 120) %>%
  ggplot(.) + 
  aes(Level) + 
  geom_histogram() + 
  facet_wrap(~Skill)

dfwide <- df %>%
  select(-Rank)

dfwide<- reshape(dfwide , idvar = "Player", timevar = "Skill", direction = "wide")
dfwide <- clean_names(dfwide)

head(dfwide)
str(dfwide)
# Liner Regression Data Check 

dflevel <- dfwide %>% select(-contains("xp"))
playerOvr <- dfwide %>% select(player, total_xp_overall)
new <- dflevel %>% left_join(playerOvr)
str(new)
head(new)

mod <- lm(total_xp_overall ~. - player -level_overall, data = new)
summary(mod)

mod <- lm(total_xp_overall ~ level_atk + level_cooking, data = dfwide)
summary(mod)


# Clustering 
# EFA
# Type of player: Pure, Iron man and etc


head(new)

tmp <- new %>% select(level_atk, level_def,
                      level_str, level_constiution, 
                      level_overall, level_crafting,
                      level_fletching)
tmp <- tmp %>%
  mutate(level_atk = as.numeric(level_atk),
         level_def = as.numeric(level_def),
         level_str = as.numeric(level_str),
         level_constiution = as.numeric(level_constiution ),
         level_overall = as.numeric(level_overall),
         level_crafting = as.numeric(level_crafting), 
         level_fletching = as.numeric(level_fletching))


ggpairs(tmp)



