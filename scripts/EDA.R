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


# Merging the web scrapped datasets of
# the Compare Page of players into one datset  

dataList <- vector("list", 16)

for(i in 1:16){
  dataList[[i]] <- read.csv(paste0('Data/ComparePage/NewData',i,".csv"))
}

# Now its in long format 
df <- do.call(rbind, dataList)
head(df, 4)

# Treating as characters for string regex 
df <- df %>%
  mutate(Rank = as.character(Rank),
         Total.XP = as.character(Total.XP),
         Level = as.character(Level))

# Removing the "," 
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

# Removing Rank because it is too telling of your total xp 
dfwide <- df %>%
  select(-Rank)

# Moving from long to wide so each row is one player in the dataset 
dfwide<- reshape(dfwide , idvar = "Player", timevar = "Skill", direction = "wide")

# cleaning the column names 
dfwide <- clean_names(dfwide)

head(dfwide)
str(dfwide)

# Dropping All Xp expect the overall  from the dataset 
dflevel <- dfwide %>% select(-contains("xp"))
playerOvr <- dfwide %>% select(player, total_xp_overall)
new <- dflevel %>% left_join(playerOvr)

# Now we are left with all the players 
# levels with there total xp of the given player 

head(new)

# Liner Regression Data Check 
mod <- lm(total_xp_overall ~. - player -level_overall, data = new)
summary(mod)


# Featuring Enginering 

# EFA took too long there isn't evaraince to seperate 
df.clust <- df.fact<-  new 


# Clustering 

# Treating NAs with zero 
# Clustering was giving number with dealing with NA
df.clust[is.na(df.clust)] <- 0

# Max distance between points to make clusters 
dis = dist(df.clust, method = "maximum")

# Making the denogram
hc1 = hclust(dis, method = "complete")

# Plotting 
plot(hc1, cex = 0.6 , hang = -1, labels = df.clust$player ,
     xlab = "Den")

# Not really that good but well cut the tree at 4 subgroups 
sub_grp = cutree(hc1, k = 4)
clustersss = df.clust %>% mutate(cluster = sub_grp) 
clustersss <- clustersss  %>% select(player, cluster)

new <- new %>% left_join(clustersss, by = 'player' )


# Type of player: Pure, Iron man and etc
# We are generalizing these skills of the players 
# into these types of pures.
# They may not accuattly be X type of pure 

# Usaully with pures defence is a hard cut off so that means
# if they have 1 def, 25, and  etc and have like 99 atk and etc 
# they are usaully a pure

# Skills below 15 don't show up no the leader baords 
# so we will assume they are a level one 
# the xp differnece between 1 and 14 isnt that much 
# our pures maybe not be true pure but they will be close enough 
# to the truth 


# new[is.na(new)] <-1 Do all the columns besides Total xp 

### Combat pures
# Basic Member's Pure: 60 Attack, 80+ Strength, 82+ Magic, 80+ Range, 1 Defence
new.copy <- new

names(new)

new  <- new %>%
  mutate(Basic_member_pure = ifelse( level_atk == 60 & 
                                       level_str >=80 &
                                       level_magic >=82 & 
                                       level_range >= 80 &
                                       level_def == 1, 1, 0))



# Obby Mauler Pure [EOC]: 1 attack, 60+ Strength, 1 Defence.
new  <- new %>%
  mutate(Obby_Mauler_Pure = ifelse( level_atk == 1 & 
                                       level_str >=60 &
                                       level_def == 1, 1, 0))

# Attack Pure [EOC]: 60+ Attack, 1 Strength, 1 Defence

new  <- new %>%
  mutate(Obby_Mauler_Pure = ifelse( level_atk >= 60 & 
                                      level_str == 1 &
                                      level_def == 1, 1, 0))


# Black Pure: 25 Defence, 60+ Attack, 80+ Strength, 
# 75+ Constitution, 80+ Range, 82+ Mage

new  <- new %>%
  mutate(Black_Pure = ifelse( level_atk >= 60 & 
                                       level_str >=80 &
                                       level_magic >=82 & 
                                       level_range >= 80 &
                                       level_def == 25 &
                                level_constiution >= 75, 1, 0))



#Turmoil/Proselyte Pure: 60+ Attack, 99 Strength, 94 Magic,
#80+ Range, 28-35 Defence, 95 Prayer. 
new  <- new %>%
  mutate(Turmoil_Proselyte_Pure = ifelse( level_atk >= 60 & 
                                level_str == 99 &
                                level_magic == 94 & 
                                level_range >= 80 &
                                level_def %in% c(25:35) &
                                  level_prayer == 95, 1, 0))

# Barrows Pure: 70+ attack, 70+ strength, 
# 70 defence, 94 magic, 70+ Prayer. 

new  <- new %>%
  mutate(Barrows_Pure = ifelse(level_atk >= 70  & 
                                 level_str >= 70 &
                                            level_magic == 94 & 
                                            level_def ==  70 &
                                            level_prayer >=70, 1, 0))



# Anti Pure [EOC]: 99 Attack, 1 Strength, 
#99 Defence, 1/52/99 Prayer, 1/50 Magic, 
#1/50 Range, 99 Constitution, 1/88/99 Summoning
names(new)
new  <- new %>%
  mutate(Anti_Pure = ifelse(level_atk == 99 & 
                                 level_str == 1 &
                                 level_magic %in% c(1,50) & 
                                 level_def ==  99 &
                                 level_prayer %in% c(1,52,99) &
                              level_range %in% c(1,50) &
                              level_summoning %in% c(1,88,99), 1, 0))



# Summoning Tank/Defence Pure [EOC]: 99 Defence,
# 1 Strength, 1 Attack, 1 Magic, 1 Range, 1/43/75/99 Prayer, 
# 99 Constitution, 1/99 Summoning. 


new  <- new %>%
  mutate(Summoning_Tank_Defence = ifelse(level_atk == 1 & 
                              level_str == 1 &
                              level_magic ==1 & 
                              level_def ==  99 &
                              level_prayer %in% c(1,43,75,99) &
                              level_range == 1 &
                              level_summoning %in% c(1,99) &
                                level_constiution == 99, 1, 0))


# Skilling Pure

#Level 3 This is the generic skiller;
#they have 10 Constitution and all combat skills are level 1.
#These are the most commonly found type of skiller.




# Number of 99s

# Number of 120s



# lightXGBoost 
# Must Move to new Script

library(tidymodels)
library(fastshap)

small <- new %>% sample_n(4000)

vb_split <- initial_split(small)
vb_train <- training(vb_split)
vb_test <- testing(vb_split)



# XGBoost 
xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(), mtry = tune(),
  learn_rate = tune(),
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")


xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), new ),
  learn_rate(),
  size = 3
)

xgb_wf <- workflow() %>%
  add_formula(total_xp_overall  ~ . - player) %>%
  add_model(xgb_spec)


vb_folds <- vfold_cv(vb_train)


doParallel::registerDoParallel()

xgb_res <- tune_grid(
  xgb_wf,
  resamples = vb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

# Run
best_auc <- select_best(xgb_res, "rmse")
final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc )

final_xgb %>%
  fit(data = df) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")


