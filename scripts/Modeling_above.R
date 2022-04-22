library(tidymodels)
library(fastshap)
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

df <- read.csv('Data/feature_eng_above_5_99s_data.csv')

vb_split <- initial_split(df)
vb_train <- training(vb_split) %>% select(-player, -level_overall)
vb_test <- testing(vb_split) %>% select(-player, -level_overall)

mod <- lm(total_xp_overall ~., data = vb_train)

pred <- predict(mod , newdata = vb_test)

print(sprintf("MSE=%0.2f", sum(mod$residuals^2)/mod$df.residual))

