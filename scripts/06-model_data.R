#### Preamble ####
# Purpose: Model voting patterns based on poll data for Democrat and Republican voters
# Author: Gadiel David Flores
# Date: 27 October 2024
# Contact: davidgadiel.flores@mail.utoronto.ca
# License: MIT

#### Updated Workspace Setup ####
library(here)
library(arrow)
library(dplyr)
library(brms)

# Data Import
analysis_data <- read_parquet(here::here("data/02-analysis_data/timeline_data.parquet"))

#### Model Setup for Democrat Voters ####

# Define updated predictor columns
predictor_columns <- c(
  "What is the highest level of education you have attained? College graduate",
  "What is the highest level of education you have attained? High school or less",
  "Can you please tell me your gender? Female",
  "Can you please tell me your gender? Male",
  "For statistical purposes only, can you please tell me your ethnicity? White or Caucasian",
  "For statistical purposes only, can you please tell me your ethnicity? Hispanic or Latino of any race",
  "Although you are undecided, which candidate do you lean toward? Donald Trump",
  "Although you are undecided, which candidate do you lean toward? Kamala Harris",
  "Who did you vote for in the 2020 election? Donald Trump",
  "Who did you vote for in the 2020 election? Joe Biden"
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

# Adjusted priors for the Democrat Model
priors_dem <- c(
  prior(normal(0, 5), class = "b"),
  prior(normal(0, 1), class = "b", coef = "Who.did.you.vote.for.in.the.2020.election..Joe.Biden"),
  prior(gamma(2, 0.1), class = "phi")  # Use gamma prior for precision parameter
)

# Fit the Democrat Model with updated control parameters
dem_model <- brm(
  formula = formula,
  data = model_data_dem,
  family = Beta(),
  prior = priors_dem,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = 4,
  seed = 123,
  control = list(adapt_delta = 0.9999, max_treedepth = 20)
)

### Save the Democrat Model ###
if (!dir.exists("models")) {
  dir.create("models")
}
saveRDS(dem_model, file = "models/bayesian_model_dem.rds")

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

# Adjusted priors for the Republican Model
priors_rep <- c(
  prior(normal(0, 5), class = "b"),
  prior(normal(0, 1), class = "b", coef = "Who.did.you.vote.for.in.the.2020.election..Donald.Trump"),
  prior(gamma(2, 0.1), class = "phi")  # Use gamma prior for precision parameter
)

# Fit the Republican Model with updated control parameters
rep_model <- brm(
  formula = formula,
  data = model_data_rep,
  family = Beta(),
  prior = priors_rep,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = 4,
  seed = 123,
  control = list(adapt_delta = 0.9999, max_treedepth = 20)
)

### Save the Republican Model ###
saveRDS(rep_model, file = "models/bayesian_model_rep.rds")
