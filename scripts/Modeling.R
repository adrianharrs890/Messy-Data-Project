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

modelingdf <- read.csv('Data/feature_eng_data.csv')

mod <- lm(total_xp_overall ~. - player -level_overall, data = modelingdf)

summary(mod)
