#### Preamble ####
# Purpose: Simulates a dataset of American electoral polling data summary
# Author: Gadiel David Flores
# Date: 07 October 2024
# Contact: davidgadiel.flores@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed
# Any other information needed? Make sure you are in the `starter_folder` rproj


#### Workspace setup ####
library(tidyverse)
set.seed(853)


#### Simulate data ####
# State names
states <- c(
  "California",
  "Texas",
  "Florida",
  "New York",
  "Pennsylvania",
  "Illinois",
  "Ohio",
  "Georgia",
  "North Carolina",
  "Michigan"
)

# Political parties
parties <- c("Republican", "Democrat", "Other")

# Other parameters
methodologies <- c("Phone", "Online", "Mixed")
region_types <- c("Urban", "Suburban", "Rural")

# Create a dataset by randomly assigning states and parties to divisions
analysis_data <- data.frame(
  Date = seq(as.Date("2023-06-01"), as.Date("2023-08-31"), length.out = 10),
  State = states,
  Candidate_A = round(runif(10, 40, 60), 2), 
  Candidate_B = round(runif(10, 40, 60), 2), 
  Sample_Size = sample(500:2000, 10, replace = TRUE), 
  Polling_Methodology = sample(methodologies, 10, replace = TRUE),
  Region_type = sample(region_types, 10, replace = TRUE), 
  Historical_result = sample(parties, 10, replace = TRUE), 
  GDP = round(runif(10, 1.0, 3.0), 2),
  Unemployment = round(runif(10, 3.0, 7.0), 2) 
)

#### Save data ####
write_csv(analysis_data, "data/00-simulated_data/simulated_data.csv")
