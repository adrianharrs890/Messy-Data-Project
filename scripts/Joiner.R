setwd("/Users/adrianharris/Desktop/MDML-Project")


# Attack
dataList <- vector("list", 9)

for(i in 1:9){
  dataList[[i]] <- read_csv(paste0("Data/Atk/RunescapeAtkLeaderboard",i, ".csv"))
}

atk <- do.call(rbind, dataList)

atk$skill <- "Attack"
atk$Player <- gsub(pattern=" ",replacement="+",x=atk$Player)


Players <- unique(atk$Player)

ggplot(atk) + 
  aes(Level) + 
  geom_histogram()


# Fix all paths below 

# Strength
str <- read_csv('Data/Strength/RunescapeStrengthLeaderboard1.csv')
str$skill <- "Strength"

# Constitution
consti <- read.csv('Data/Constitution/RunescapeConstitLeaderboard1.csv')
consti$skill <- "Constitution"


# Cooking
cooking <- read.csv('RunescapeCookingLeaderboard1.csv')
cooking$skill <- "Cooking"

tmp <- merge(atk, consti, by = "Player")


head(tmp)
View(tmp)

