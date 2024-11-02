#### Preamble ####
# Purpose: Cleans the raw presidential general election polls data into usable links
# an analysis dataset
# Author: Tina Kim
# Date: 7 October 2024
# Contact: tinak.kim@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need to have downloaded the data
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)

#### Clean Raw data ####
raw_data <- read_csv("data/01-raw_data/raw_data.csv")

# Find the top 3 most frequent pollster in the raw data
top_pollsters <- raw_data %>%
  count(pollster, sort = TRUE) %>%
  top_n(3, n) %>%
  pull(pollster)

cleaned_data <- raw_data %>%
  # Filter the raw data to only top 3 most frequent pollster names
  filter(pollster %in% top_pollsters) %>%
  # Remove columns with completely empty rows
  select(where(~ any(!is.na(.)))) 

# Calculate missing values for each of the three pollsters
missing_values_summary <- cleaned_data %>%
  group_by(pollster) %>%
  summarize(
    # Missing number of variables across all columns for each pollster
    missing_count = sum(is.na(across(everything()))),
    # Total number of variables for each pollster
    total_count = n() * ncol(cleaned_data),
    .groups = 'drop'
  ) %>%
  # (Missing # of variables / total # of variables) ratio for each pollster
  mutate(missing_ratio = missing_count / total_count) %>%
  # Order from smallest to biggest ratio value
  arrange(missing_ratio)

# Find the pollster with the lowest missing values ratio because it has
# the most complete data which could lead to a more reliable analysis
smallest_ratio_pollster <- missing_values_summary %>%
  slice(1) %>%
  pull(pollster)

# Filter cleaned data to include only the smallest ratio pollster.
# This pollster is a good balance of high number of observations and a low
# number of missing values.
cleaned_data <- cleaned_data %>%
  filter(pollster == smallest_ratio_pollster)

#### Save data ####
write_csv(cleaned_data, "data/02-analysis_data/pollster_data/emerson_list_data.csv")
