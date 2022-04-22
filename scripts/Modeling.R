# RF
# lightXGBoost 
# XGBoost 
# Our data blows up my computer 

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

small <- new %>% sample_n(4000)

vb_split <- initial_split(small)
vb_train <- training(vb_split)
vb_test <- testing(vb_split)


tree_rec <- recipe(legal_status ~ ., data = trees_train) 




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

