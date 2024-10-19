# Load necessary libraries
library(dplyr)
library(lubridate)
library(readr)
library(janitor)

# Read and clean the data
data <- read_csv("data/02-analysis_data/survey_list_data.csv") |>
  clean_names()

# Clean the data (treat NA state as National, convert dates to date format)
cleaned_data <- data |>
  mutate(
    state = if_else(is.na(state), "National", state), # Treat NA as National
    end_date = mdy(end_date), # Convert end_date to date format
    start_date = mdy(start_date) # Convert start_date to date format if needed
  ) |>
  mutate(
    num_respondents = round((pct / 100) * sample_size, 0) # Convert percentage to number (applies to any candidate)
  )

# Filter to include only Democrats (DEM) and Republicans (REP)
filtered_data <- cleaned_data |>
  filter(party %in% c("DEM", "REP"))

# Extract relevant information for each unique state and party combination
state_summary <- filtered_data |>
  group_by(state, party) |>
  filter(end_date == max(end_date, na.rm = TRUE)) |>
  summarize(
    start_date = min(start_date, na.rm = TRUE), # Earliest start date for each state and party
    end_date = max(end_date, na.rm = TRUE), # Latest end date for each state and party
    total_sample_size = sum(sample_size, na.rm = TRUE), # Total sample size for each state and party
    pct = first(pct), # Use the pct value associated with the latest entry
    num_respondents = sum(num_respondents, na.rm = TRUE) # Sum of number of respondents for each state and party
  ) |>
  ungroup()

# Read existing cleaned dataset
cleaned_dataset <- read_csv("data/02-analysis_data/non_missing_state_poll_data.csv")

# Create separate columns for Democrats and Republicans
pivoted_data <- state_summary %>%
  select(state, party, start_date, end_date, total_sample_size, pct, num_respondents) %>%
  pivot_wider(
    names_from = party,
    values_from = c(start_date, end_date, total_sample_size, pct, num_respondents),
    names_glue = "{.value}_{party}"
  ) %>%
  rename(
    democrat_start_date = start_date_DEM,
    democrat_end_date = end_date_DEM,
    democrat_total_sample_size = total_sample_size_DEM,
    democrat_pct = pct_DEM,
    democrat_num_respondents = num_respondents_DEM,
    
    republican_start_date = start_date_REP,
    republican_end_date = end_date_REP,
    republican_total_sample_size = total_sample_size_REP,
    republican_pct = pct_REP,
    republican_num_respondents = num_respondents_REP
  )

# Join data with cleaned dataset
updated_dataset <- cleaned_dataset %>%
  left_join(pivoted_data, by = "state")

# Reorder the columns
updated_dataset <- updated_dataset %>%
  select(state, starts_with("democrat"), starts_with("republican"), everything())

# Save dataset
write_csv(updated_dataset, "data/02-analysis_data/organized_state_poll_data.csv")
