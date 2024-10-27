#### Preamble ####
# Purpose: Organizes states poll links
# an analysis dataset
# Author: Gadiel David Flores Jimenez
# Date: 7 October 2024
# Contact: davidgadiel.flores@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need to cleaned poll data
# Any other information needed? Intall googledrive and googleAuthR
library(tidyverse)

# Load the poll links data
poll_links_data <- read_csv("data/02-analysis_data/pollster_data/emerson_list_data.csv")

#### Pivot for URL crosstabs ####
url_pivot <- poll_links_data %>%
  group_by(state) %>%
  mutate(url_rank = row_number()) %>%
  select(state, url_crosstab, url_rank) %>%
  pivot_wider(
    names_from = url_rank,
    values_from = url_crosstab,
    names_prefix = "url_crosstab_"
  ) %>%
  ungroup()

#### Shift non-NA values to the left in relevant columns ####
# Define columns to shift
columns_to_shift <- names(url_pivot)[!(names(url_pivot) %in% c("state"))]

# Function to shift values to the left
shift_left <- function(row) {
  non_na_values <- row[!is.na(row)]  # Keep only non-NA values
  c(non_na_values, rep(NA, length(row) - length(non_na_values)))  # Fill the remaining with NA
}

# Apply shift_left function
new_poll_data_clean <- url_pivot %>%
  mutate(across(all_of(columns_to_shift), ~ shift_left(.)))

#### Remove columns that are NA ####
new_poll_data_clean <- new_poll_data_clean %>%
  select(where(~ any(!is.na(.)))) 

#### Save the cleaned dataset ####
write_csv(new_poll_data_clean, "data/02-analysis_data/pollster_data/pollster_link_data.csv")


