#### Preamble ####
# Purpose: Simulates a dataset of American electoral polling data summary with trends across dates
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

# Helper function to simulate a linear trend over dates
generate_trend <- function(start, end, n) {
  return(seq(start, end, length.out = n))
}

# Define states and dates for each entry
states <- c("California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia", "North Carolina", "Michigan")
dates <- as.Date(c("2024-01-15", "2024-05-15", "2024-09-15"))  # Specific dates
n_dates <- length(dates)  # Number of date points

# Expand the dataset to include each state for each date
analysis_data <- expand.grid(State = states, Date = dates, stringsAsFactors = FALSE)

# Simulate percentages for each response type with trends
analysis_data <- analysis_data %>%
  mutate(
    # Trend: Increase Trump choice over time from 45% to 55%
    Presidential_Choice_Trump = rep(generate_trend(45, 55, n_dates), each = length(states)) + rnorm(nrow(analysis_data), 0, 1),
    # Trend: Decrease Harris choice complement to Trump choice
    Presidential_Choice_Harris = 100 - Presidential_Choice_Trump,
    
    # Trend: Biden's disapproval increases from 50% to 60%
    Joe_Biden_Approval_Disapprove = rep(generate_trend(50, 60, n_dates), each = length(states)) + rnorm(nrow(analysis_data), 0, 1),
    # Biden's approval is complementary to disapproval
    Joe_Biden_Approval_Approve = 100 - Joe_Biden_Approval_Disapprove,
    
    # Trend: Perceived safety becomes less safe, decreasing from 40% to 30%
    Safety_Concerns_More_Safe = rep(generate_trend(40, 30, n_dates), each = length(states)) + rnorm(nrow(analysis_data), 0, 1),
    Safety_Concerns_Less_Safe = 100 - Safety_Concerns_More_Safe,
    
    # Trend: Economic situation worsens from 40% better to 30% better
    Economic_Situation_Better = rep(generate_trend(40, 30, n_dates), each = length(states)) + rnorm(nrow(analysis_data), 0, 1),
    Economic_Situation_Worse = 100 - Economic_Situation_Better,
    
    # Consistent past voting behavior
    Past_Voting_Behavior_Biden = 52 + rnorm(nrow(analysis_data), 0, 1),
    Past_Voting_Behavior_Trump = 100 - Past_Voting_Behavior_Biden,
    
    # Consistent ethnicity distribution (Hispanic ~15%, White ~60%)
    Ethnicity_Hispanic = 15 + rnorm(nrow(analysis_data), 0, 0.5),
    Ethnicity_White = 60 + rnorm(nrow(analysis_data), 0, 0.5)
  )

# Ensure percentage limits are respected by trimming values outside 0-100 range
analysis_data <- analysis_data %>%
  mutate(
    across(starts_with("Joe_Biden_Approval"), ~ pmin(pmax(., 0), 100)),
    across(starts_with("Presidential_Choice"), ~ pmin(pmax(., 0), 100)),
    across(starts_with("Safety_Concerns"), ~ pmin(pmax(., 0), 100)),
    across(starts_with("Economic_Situation"), ~ pmin(pmax(., 0), 100)),
    Past_Voting_Behavior_Biden = pmin(pmax(Past_Voting_Behavior_Biden, 0), 100),
    Past_Voting_Behavior_Trump = pmin(pmax(Past_Voting_Behavior_Trump, 0), 100),
    Ethnicity_Hispanic = pmin(pmax(Ethnicity_Hispanic, 0), 100),
    Ethnicity_White = pmin(pmax(Ethnicity_White, 0), 100)
  )


#### Save data ####
write_csv(analysis_data, "data/00-simulated_data/simulated_poll_data.csv")


