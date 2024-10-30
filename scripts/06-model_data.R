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
library(brms)
library(caret)
library(pROC)
library(glmnet)

# Data Import
analysis_data1 <- read_parquet(here::here("data/02-analysis_data/summarized_state_poll_data.parquet"))
analysis_data2 <- read_parquet(here::here("data/02-analysis_data/timeline_data.parquet"))

#### Define Predictors and Target Variable ####

# Column references provided
column_refs <- list(
  democrat_pct = "democrat_pct",
  republican_pct = "republican_pct",
  highly_educated = "What is the highest level of education you have attained? College graduate",
  lower_educated = "What is the highest level of education you have attained? High school or less",
  female = "Can you please tell me your gender? Female",
  male = "Can you please tell me your gender? Male",
  non_binary_other = "Can you please tell me your gender? Nonbinary or other",
  caucasian = "For statistical purposes only, can you please tell me your ethnicity? White or Caucasian",
  hispanic_latino = "For statistical purposes only, can you please tell me your ethnicity? Hispanic or Latino of any race",
  vote_trump = "Who did you vote for in the 2020 election? Donald Trump",
  vote_biden = "Who did you vote for in the 2020 election? Joe Biden",
  biden_approve = "Do you approve or disapprove of the job Joe Biden is doing as President? Approve",
  biden_disapprove = "Do you approve or disapprove of the job Joe Biden is doing as President? Disapprove",
  previous_vote_trump = "Who did you vote for in the 2020 election? Donald Trump",
  previous_vote_joe = "Who did you vote for in the 2020 election? Joe Biden"
)

# Unlist column references to get a vector of column names
predictor_columns <- unlist(column_refs)

# Ensure all required columns are present
missing_columns <- setdiff(predictor_columns, names(analysis_data1))
if (length(missing_columns) > 0) {
  stop("The following required columns are missing in the dataset: ", paste(missing_columns, collapse = ", "))
}

# Define the target variable
analysis_data1 <- analysis_data1 %>%
  mutate(
    democratic_win = ifelse(democrat_pct > republican_pct, 1, 0)
  )

#### Adjust Biden's Approval Ratings ####
biden_weight <- 0.5  # Adjust this value as needed
analysis_data1 <- analysis_data1 %>%
  mutate(
    `Do you approve or disapprove of the job Joe Biden is doing as President? Approve` = 
      `Do you approve or disapprove of the job Joe Biden is doing as President? Approve` * biden_weight,
    `Do you approve or disapprove of the job Joe Biden is doing as President? Disapprove` = 
      `Do you approve or disapprove of the job Joe Biden is doing as President? Disapprove` * biden_weight
  )

#### Prepare Data for Modeling ####
model_data <- analysis_data1 %>%
  select(all_of(predictor_columns), democratic_win) %>%
  mutate(across(everything(), as.numeric))

if (any(is.na(model_data))) {
  model_data <- na.omit(model_data)
}

# Standardize column names to remove spaces and special characters
colnames(model_data) <- gsub("[^[:alnum:]_]", "_", colnames(model_data))
predictor_columns <- colnames(model_data)[colnames(model_data) %in% predictor_columns]

# Define the model formula
formula <- as.formula(paste("democratic_win ~", paste(predictor_columns, collapse = " + ")))

# Specify priors (you can adjust these based on prior knowledge)
priors <- set_prior("normal(0, 10)", class = "b")

#### Fit the Bayesian Logistic Regression Model ####
first_model <- brm(
  formula = formula,
  data = model_data,
  family = bernoulli(link = "logit"),
  prior = priors,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 2,
  seed = 123
)

#### Model Diagnostics and Summary ####
print(summary(first_model))
plot(first_model)
pp_check(first_model)

#### Save the Model ####

# Create the 'models' directory if it doesn't exist
if (!dir.exists("models")) {
  dir.create("models")
}

saveRDS(
  first_model,
  file = "models/bayesian_model.rds"
)


