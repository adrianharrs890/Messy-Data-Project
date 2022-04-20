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


df.clust <- df.fact<-  new 
# EFA
library(haven)
library(factoextra)
library(psych)

for(i in 1:length(df.fact)){
  df.fact[, i] <- as.numeric(df.fact[, i])
}

R = cor(df.fact, use = "complete.obs")

fa1 <- fa(df.fact, nfactors = 9, fm = "pa", rotate = "none", SMC = F)

KMO(R)

plot(fa1$e.values, type = "b", main = "Screeplot")


fa2_rotated  <- fa(r = df.fact, nfactors = 3,  fm = "pa", rotate = "varimax", SMC = F)

fit.R <- fa2_rotated$fit.off
psych::alpha(x = df.fact)


# Factorx
plot(fa2_rotated$loadings, type = "n", xlim = c(-.8, 1.4), ylim = c(-.9,1))
text(fa2_rotated$loadings,labels=names(df.fact),cex=.7)
abline(v = 0, h = 0)


# Clustering 





# Type of player: Pure, Iron man and etc

# Number of 99s




11/3


head(new)
3 + 4 + 4




