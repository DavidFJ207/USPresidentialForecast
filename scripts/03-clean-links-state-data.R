library(tidyverse)

# Load the poll links data
poll_links_data <- read_csv("data/02-analysis_data/survey_list_data.csv")


# Create a new dataset with unique states and URLs and their corresponding party columns
new_poll_data <- poll_links_data %>%
  # Group by state
  group_by(state) %>%
  # Rank the URLs for each state
  mutate(url_rank = row_number()) %>%
  # Select relevant columns
  select(state, url_crosstab, sample_size, party, url_rank) %>%
  # Spread URLs and their corresponding parties into separate columns based on the rank
  pivot_wider(
    names_from = url_rank,
    values_from = c(url_crosstab, party),
    names_prefix = c("url_crosstab_", "party_")
  ) %>%
  # Calculate the average sample size for each state
  group_by(state) %>%
  summarize(
    across(starts_with("url_crosstab"), ~ first(na.omit(.)), .names = "{col}"),
    across(starts_with("party"), ~ first(na.omit(.)), .names = "{col}"),
    average_sample_size = mean(sample_size, na.rm = TRUE)
  ) %>%
  ungroup()

# Optionally, save the new dataset to a CSV file
write_csv(new_poll_data, "data/02-analysis_data/processed_poll_data.csv")
