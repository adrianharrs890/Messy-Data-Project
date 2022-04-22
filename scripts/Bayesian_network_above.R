library(tidymodels)
library(fastshap)
library(tidyverse)
library(rvest)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(janitor)
library(bnlearn)
library(bnviewer)

rm(list = ls())
set.seed(1234)
options(scipen=999)

setwd("/Users/adrianharris/Desktop/Messy-Data-Project")

df <- read.csv('Data/feature_eng_above_5_99s_data.csv')

for(i in 2:length(df)){
  df[,i] <- as.numeric(df[,i])
}


bg <- hc(df)

viewer(bg,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_with_sugiyama",
       bayesianNetwork.title="Above 5 99s",
       bayesianNetwork.subtitle = "First Network",
       bayesianNetwork.footer = "Fig. 1"
)
