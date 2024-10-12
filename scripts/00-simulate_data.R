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

# Helper function to generate random percentages for responses that add up to 100%
generate_percentages <- function(n) {
  p <- runif(n)
  return(round(p / sum(p) * 100, 2))  # Normalize to sum to 100% and round to 2 decimal places
}

# Simulate data
states <- c("California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia", "North Carolina", "Michigan")

# Simulate percentages for each response type outside the data.frame
Joe_Biden_Approval_Approve <- generate_percentages(10)
Joe_Biden_Approval_Disapprove <- 100 - Joe_Biden_Approval_Approve

Presidential_Choice_Harris <- generate_percentages(10)
Presidential_Choice_Trump <- 100 - Presidential_Choice_Harris

Safety_Concerns_More_Safe <- generate_percentages(10)
Safety_Concerns_Less_Safe <- 100 - Safety_Concerns_More_Safe

Economic_Situation_Better <- generate_percentages(10)
Economic_Situation_Worse <- 100 - Economic_Situation_Better

Past_Voting_Behavior_Biden <- generate_percentages(10)
Past_Voting_Behavior_Trump <- 100 - Past_Voting_Behavior_Biden

Ethnicity_Hispanic <- generate_percentages(10)
Ethnicity_White <- 100 - Ethnicity_Hispanic

# Create the dataset by including the simulated data
analysis_data <- data.frame(
  State = states,  # No replacement to ensure unique states
  Joe_Biden_Approval_Approve = Joe_Biden_Approval_Approve,
  Joe_Biden_Approval_Disapprove = Joe_Biden_Approval_Disapprove,
  Presidential_Choice_Harris = Presidential_Choice_Harris,
  Presidential_Choice_Trump = Presidential_Choice_Trump,
  Safety_Concerns_More_Safe = Safety_Concerns_More_Safe,
  Safety_Concerns_Less_Safe = Safety_Concerns_Less_Safe,
  Economic_Situation_Better = Economic_Situation_Better,
  Economic_Situation_Worse = Economic_Situation_Worse,
  Past_Voting_Behavior_Biden = Past_Voting_Behavior_Biden,
  Past_Voting_Behavior_Trump = Past_Voting_Behavior_Trump,
  Ethnicity_Hispanic = Ethnicity_Hispanic,
  Ethnicity_White = Ethnicity_White
)

#### Save data ####
write_csv(analysis_data, "data/00-simulated_data/simulated_poll_data.csv")

