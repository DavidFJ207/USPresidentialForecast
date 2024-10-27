#### Preamble ####
# Purpose: Models voting patterns based on poll data
# Author: Gadiel David Flores
# Date: 27 October 2024
# Contact: davidgadiel.flores@mail.utoronto.ca
# License: MIT
# Pre-requisites: summarized_state_poll_data.parquet should be available in the data directory


#### Workspace setup ####
library(here)
library(arrow)      
library(dplyr)
library(caret)      
library(pROC)       
library(glmnet)     

# Data Import
analysis_data1 <- read_parquet(here::here("data/02-analysis_data/summarized_state_poll_data.parquet"))

#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)


