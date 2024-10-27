#### Preamble ####
# Purpose: Extracts data from links into a summarize dataset
# an analysis dataset
# Author: Gadiel David Flores
# Date: 7 October 2024
# Contact: davidgadiel.flores@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need to cleaned poll data
# Any other information needed? Intall googledrive and googleAuthR

# Load necessary libraries
library(dplyr)
library(lubridate)
library(readr)
library(janitor)
library(arrow)

#### Merge datasets ####
# Read and clean the data
data <- read_csv("data/02-analysis_data/pollster_data/emerson_list_data.csv") |>
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
cleaned_dataset <- read_csv("data/02-analysis_data/pollster_data/summarized_questions_data.csv")

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
write_parquet(updated_dataset, "data/02-analysis_data/summarized_state_poll_data.parquet")
#### Create Timeline Dataset ####

# Load main dataset
state_data <- data
# Initialize an empty data frame for timeline_data
timeline_data <- data.frame(state = character(),
                            start_date = character(),
                            end_date = character(),
                            party = character(),
                            pct = numeric(),
                            `Do you approve or disapprove of the job Joe Biden is doing as President? Disapprove` = character(),
                            `Do you approve or disapprove of the job Joe Biden is doing as President? Approve` = character(),
                            stringsAsFactors = FALSE,
                            check.names = FALSE)



# Iterate through each row in state_data
for (i in seq_len(nrow(state_data))) {
  # Extract state information and url link
  state_value <- as.character(state_data$state[i])
  start_date <- as.character(state_data$start_date[i])
  end_date <- as.character(state_data$end_date[i])
  party <- as.character(state_data$party[i])
  pct <- as.numeric(state_data$pct[i])
  link <- as.character(state_data$url_crosstab[i])
  
  # Check if the link is not NA
  if (!is.na(link)) {
    # Define a placeholder for processed data
    processed_data <- NULL
    
    # Determine the type of link and process accordingly
    tryCatch({
      if (is_google_sheet_link(link)) {
        # Process Google Sheets link
        processed_data <- process_google_sheet(link, output_path = NULL)
      } else if (is_excel_link(link)) {
        # Process Excel file link
        processed_data <- process_excel_sheet(link, output_path = NULL)
      }
      
      # If processed_data is obtained, append it to timeline_data
      if (!is.null(processed_data)) {
        # Loop through rows of processed data
        for (j in seq_len(nrow(processed_data))) {
          # Identify the question and response
          question <- as.character(processed_data[j, 1])
          response <- as.character(processed_data[j, 3])
          
          # Check if the question is in the list of target questions
          if (question %in% colnames(timeline_data)) {
            # Create a new row with the data to append
            new_row <- data.frame(state = state_value,
                                  start_date = start_date,
                                  end_date = end_date,
                                  party = party,
                                  pct = pct,
                                  stringsAsFactors = FALSE)
            new_row[[question]] <- response  # Add the response to the question column
            
            # Append the new row to timeline_data
            timeline_data <- rbind(timeline_data, new_row)
          }
        }
      }
    }, error = function(e) {
      # Handle errors and continue the loop
      message(paste("Error processing link:", link, "-", e$message))
    })
  }
}

# Final timeline_data is now ready with organized survey responses by state, date, party, and pct.
