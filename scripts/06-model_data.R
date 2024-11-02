#### Preamble ####
# Purpose: Model voting patterns based on poll data for Democrat and Republican voters
# Author: Gadiel David Flores
# Date: 27 October 2024
# Contact: davidgadiel.flores@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(here)
library(arrow)
library(dplyr)
library(brms)     

# Data Import
analysis_data <- read_parquet(here::here("data/02-analysis_data/timeline_data.parquet"))

#### Model Setup for Democrat Voters ####

# Define column references 
predictor_columns <- c(
  "What is the highest level of education you have attained? College graduate",
  "What is the highest level of education you have attained? High school or less",
  "Can you please tell me your gender? Female",
  "Can you please tell me your gender? Male",
  "For statistical purposes only, can you please tell me your ethnicity? White or Caucasian",
  "For statistical purposes only, can you please tell me your ethnicity? Hispanic or Latino of any race",
  "Who did you vote for in the 2020 election? Donald Trump",
  "Who did you vote for in the 2020 election? Joe Biden",
  "Do you approve or disapprove of the job Joe Biden is doing as President? Approve",
  "Do you approve or disapprove of the job Joe Biden is doing as President? Disapprove"
)

# Filter analysis_data to only include rows where party is DEM
analysis_data_dem <- analysis_data %>% filter(party == "DEM")

# Rename columns for modeling compatibility
names(analysis_data_dem) <- make.names(names(analysis_data_dem))
predictor_columns <- make.names(predictor_columns)

# Scale `pct` to be within (0, 1)
epsilon <- 1e-6
analysis_data_dem <- analysis_data_dem %>%
  mutate(
    pct_scaled = (pct / 100) * (1 - 2 * epsilon) + epsilon
  )

# Ensure all predictor columns are numeric and drop rows with NA
model_data_dem <- analysis_data_dem %>%
  select(all_of(c("pct_scaled", predictor_columns))) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

### Specify and Fit the Democrat Model ###

# Create the formula
predictor_formula <- paste(predictor_columns, collapse = " + ")
formula <- as.formula(paste("pct_scaled ~", predictor_formula))

# Set priors with specific priors for `Who did you vote for...` columns
priors <- c(
  prior(normal(0, 5), class = "b"),
  prior(normal(0, 1), class = "b", coef = "Who.did.you.vote.for.in.the.2020.election..Donald.Trump"),
  prior(normal(0, 1), class = "b", coef = "Who.did.you.vote.for.in.the.2020.election..Joe.Biden"),
  prior(beta(1, 1), class = "phi")  # Prior for precision parameter
)

# Fit the Democrat Model
dem_model <- brm(
  formula = formula,
  data = model_data_dem,
  family = Beta(),
  prior = priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = 4,
  seed = 123,
  control = list(adapt_delta = 0.99999, max_treedepth = 20)
)

### Save the Democrat Model ###

if (!dir.exists("models")) {
  dir.create("models")
}

saveRDS(
  dem_model,
  file = "models/bayesian_model_dem.rds"
)

#### Model Setup for Republican Voters ####

# Filter analysis_data to only include rows where party is REP
analysis_data_rep <- analysis_data %>% filter(party == "REP")

# Rename columns for modeling compatibility
names(analysis_data_rep) <- make.names(names(analysis_data_rep))

# Scale `pct` to be within (0, 1)
analysis_data_rep <- analysis_data_rep %>%
  mutate(
    pct_scaled = (pct / 100) * (1 - 2 * epsilon) + epsilon
  )

# Ensure all predictor columns are numeric and drop rows with NA
model_data_rep <- analysis_data_rep %>%
  select(all_of(c("pct_scaled", predictor_columns))) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

### Specify and Fit the Republican Model ###

# Fit the Republican Model
rep_model <- brm(
  formula = formula,
  data = model_data_rep,
  family = Beta(),
  prior = priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = 4,
  seed = 123,
  control = list(adapt_delta = 0.99999, max_treedepth = 20)
)

### Save the Republican Model ###

saveRDS(
  rep_model,
  file = "models/bayesian_model_rep.rds"
)