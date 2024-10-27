#### Preamble ####
# Purpose: Extracts data from links into a summarize dataset
# an analysis dataset
# Author: Gadiel David Flores
# Date: 7 October 2024
# Contact: davidgadiel.flores@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need to cleaned poll data
# Any other information needed? Intall googledrive and googleAuthR

#### Load Libraries and files ####
library(googledrive)
library(readxl)
library(tidyverse)
library(dplyr)
library(dplyr)
library(lubridate)
library(readr)
library(janitor)
library(arrow)
state_data <- read_csv("data/02-analysis_data/pollster_data/pollster_link_data.csv")
#### Helper Functions ####

# Function to check if a string is a Google Sheets link
is_google_sheet_link <- function(text) {
  grepl("docs.google.com/spreadsheets", text)
}

# Function to download and read a Google Sheet
download_and_read_google_sheet <- function(url) {
  # Download the file from the Google Sheets URL
  file <- drive_download(as_id(url), path = "temp_file.xlsx", overwrite = TRUE)
  # Read the file
  data <- read_excel(file$local_path)
  # Return the data
  return(data)
}

# Function to replace NA values with the previous non-NA value
replace_na_with_previous <- function(df, column_index) {
  last_non_na <- NA  # Variable to store the last non-NA value
  
  for (i in 1:nrow(df)) {
    if (is.na(df[[column_index]][i])) {
      df[[column_index]][i] <- last_non_na  # Replace NA with last non-NA value
    } else {
      last_non_na <- df[[column_index]][i]  # Update last non-NA value
    }
  }
  
  return(df)
}


#### Main Function to Process Google and Excel Sheets####
process_google_sheet <- function(sheet_url, output_path) {
  # Step 1: Download and read the Google Sheet
  sheet_data <- download_and_read_google_sheet(sheet_url)
  
  # Step 2: Define unwanted values to clean
  unwanted_values <- c("Valid", "Missing", "Total", "Combined Presidential Vote", "Donald Trump")
  
  # Step 3: Clean the first column by replacing unwanted values with NA
  cleaned_sheet_data <- sheet_data %>%
    mutate(
      first_column = if_else(sheet_data[[1]] %in% unwanted_values, NA_character_, sheet_data[[1]])
    )
  
  # Step 4: Drop the original first column and replace with the cleaned version
  cleaned_sheet_data[[1]] <- cleaned_sheet_data$first_column
  cleaned_sheet_data <- cleaned_sheet_data %>%
    select(-first_column)
  
  # Step 5: Create a new header row
  column_names_list <- colnames(sheet_data)
  new_header <- as.data.frame(matrix(ncol = length(column_names_list), nrow = 1))
  colnames(new_header) <- column_names_list
  
  
  # Step 6: Convert column names of sheet_data into a data frame and align with new header
  column_names_list <- colnames(sheet_data)
  column_names_df <- as.data.frame(t(column_names_list))
  colnames(column_names_df) <- colnames(new_header)
  colnames(cleaned_sheet_data) <- colnames(new_header)
  
  # Step 7: Bind the new header row, column names, and cleaned data together
  cleaned_sheet_data_with_header <- bind_rows(new_header, column_names_df, cleaned_sheet_data)
  
  # Step 8: Handle NA replacement in the first column (using index 1)
  cleaned_sheet_data_with_previous <- replace_na_with_previous(cleaned_sheet_data_with_header, 1)
  
  # Step 9: Remove rows where the second column (index 2) is NA
  cleaned_sheet_data_no_na <- cleaned_sheet_data_with_previous %>%
    filter(!is.na(.[[2]]))
  
  # Step 10: Merge `question_1` and `question_2`
  merged_data <- cleaned_sheet_data_no_na %>%
    mutate(merged_question = paste(.[[1]], .[[2]], sep = " ")) %>%
    select(merged_question, all_of(colnames(cleaned_sheet_data_no_na)[3:ncol(cleaned_sheet_data_no_na)]))
  
  
  # Return the final merged dataset for viewing
  return(merged_data)
}
process_excel_sheet <- function(excel_url, output_path) {
  # Step 1: Download and read the Excel file
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(excel_url, temp_file, mode = "wb")
  sheet_data <- read_excel(temp_file)
  
  # Step 2: Define unwanted values to clean
  unwanted_values <- c("Valid", "Missing", "Total", "Combined Presidential Vote", "Donald Trump")
  
  # Step 3: Clean the first column by replacing unwanted values with NA
  cleaned_sheet_data <- sheet_data %>%
    mutate(
      first_column = if_else(sheet_data[[1]] %in% unwanted_values, NA_character_, sheet_data[[1]])
    )
  
  # Step 4: Drop the original first column and replace it with the cleaned version
  cleaned_sheet_data[[1]] <- cleaned_sheet_data$first_column
  cleaned_sheet_data <- cleaned_sheet_data %>%
    select(-first_column)
  
  # Step 5: Create a new header row
  column_names_list <- colnames(sheet_data)
  new_header <- as.data.frame(matrix(ncol = length(column_names_list), nrow = 1))
  colnames(new_header) <- column_names_list
  
  # Step 6: Convert column names of sheet_data into a data frame and align with new header
  column_names_list <- colnames(sheet_data)
  column_names_df <- as.data.frame(t(column_names_list))
  colnames(column_names_df) <- colnames(new_header)
  colnames(cleaned_sheet_data) <- colnames(new_header)
  
  # Step 7: Bind the new header row, column names, and cleaned data together
  cleaned_sheet_data_with_header <- bind_rows(new_header, column_names_df, cleaned_sheet_data)
  
  # Step 8: Handle NA replacement in the first column (using index 1)
  cleaned_sheet_data_with_previous <- cleaned_sheet_data_with_header %>%
    fill(.[[1]], .direction = "down") # Replace NAs with the previous non-NA value
  
  # Step 9: Remove rows where the second column (index 2) is NA
  cleaned_sheet_data_no_na <- cleaned_sheet_data_with_previous %>%
    filter(!is.na(.[[2]]))
  
  # Step 10: Merge `question_1` and `question_2`
  merged_data <- cleaned_sheet_data_no_na %>%
    mutate(merged_question = paste(.[[1]], .[[2]], sep = " ")) %>%
    select(merged_question, all_of(colnames(cleaned_sheet_data_no_na)[3:ncol(cleaned_sheet_data_no_na)]))
  
  # Step 11: Save the final merged dataset to the output path
  write.csv(merged_data, output_path, row.names = FALSE)
  
  # Return the final merged dataset for viewing
  return(merged_data)
}
#### Create final dataset ####

# First, process the mega dataset from state_data to populate mega_dataset_combined
mega_dataset <- list()

# Iterate through the entire state_data dataframe
for (i in seq_len(nrow(state_data))) {
  for (j in seq_len(ncol(state_data))) {
    cell_value <- as.character(state_data[i, j])  # Get the cell value as a string
    
    # Check if the cell contains a Google Sheets link
    if (!is.na(cell_value) && is_google_sheet_link(cell_value)) {
      
      # Process the Google Sheet link
      tryCatch({
        processed_data <- process_google_sheet(cell_value, output_path = NULL)  # Use your process_google_sheet function
        
        # Append the processed data to the mega dataset
        mega_dataset <- append(mega_dataset, list(processed_data))
      }, error = function(e) {
        # Handle errors if any occur during the process (e.g., invalid link)
        message(paste("Error processing link:", cell_value, e))
      })
    }
  }
}

# Combine all data frames in mega_dataset into one
mega_dataset_combined <- bind_rows(mega_dataset)

# Save the combined dataset to a CSV file
write_csv(mega_dataset_combined, "data/02-analysis_data/pollster_data/questions_dataset.csv")

#### Continue with item counts ####
item_counts <- table(mega_dataset_combined[[1]])
# Convert the table to a dataframe
item_counts_df <- as.data.frame(item_counts)
# Rename the columns for clarity
colnames(item_counts_df) <- c("Item", "Count")
# Filter out rows where the count is less than 40
item_counts_filtered <- item_counts_df %>%
  filter(Count >= 49)

# Create dataset with correct headers
item_list <- as.character(item_counts_filtered$Item)
state_survey_data <- data.frame(matrix(ncol = length(item_list) + 1, nrow = 0))
colnames(state_survey_data) <- c("state", item_list)
names <- state_survey_data
state_survey_data$state <- as.character(state_survey_data$state)
state_data$state <- as.character(state_data$state)

# Now append the state column from state_data to state_survey_data
state_survey_data <- bind_rows(state_survey_data, data.frame(state = state_data$state))

#### Loop to append data ####
# Iterate through the entire state_data dataframe
for (i in seq_len(nrow(state_data))) {
  for (j in seq_len(ncol(state_data))) {
    cell_value <- as.character(state_data[i, j])  # Get the cell value as a string
    state_value <- as.character(state_data[i, 1]) # Get the state value from the current row of state_data (assuming state is in the first column)
    
    # Check if the cell contains a link
    if (!is.na(cell_value)) {
      # Check if the cell contains a Google Sheets link
      if (is_google_sheet_link(cell_value)) {
        
        # Process the Google Sheet link
        tryCatch({
          processed_data <- process_google_sheet(cell_value, output_path = NULL)  # Use your process_google_sheet function
          
          # Iterate through processed_data rows
          for (k in 1:nrow(processed_data)) {
            # Get the question from processed_data
            question_to_match <- as.character(processed_data[k, 1])  # Question (header) from processed_data
            value_to_append <- as.character(processed_data[k, 3])  # Value to append
            
            # Ensure the question matches a column in state_survey_data
            if (question_to_match %in% colnames(state_survey_data)) {
              # Get the column index of the matching question in state_survey_data
              question_index <- which(colnames(state_survey_data) == question_to_match)
              
              # Append the value to the current row (state) and matching column (question)
              state_survey_data[i, question_index] <- value_to_append
            }
          }
        }, error = function(e) {
          # Handle errors if any occur during the process (e.g., invalid link)
          message(paste("Error processing Google Sheet link:", cell_value, e))
        })
        
        # Check if the cell contains an Excel link
      } else if (is_excel_link(cell_value)) {
        
        # Process the Excel link
        tryCatch({
          processed_data <- process_excel_sheet(cell_value, output_path = NULL)  # Use your process_excel_sheet function
          
          # Iterate through processed_data rows
          for (k in 1:nrow(processed_data)) {
            # Get the question from processed_data
            question_to_match <- as.character(processed_data[k, 1])  # Question (header) from processed_data
            value_to_append <- as.character(processed_data[k, 3])  # Value to append
            
            # Ensure the question matches a column in state_survey_data
            if (question_to_match %in% colnames(state_survey_data)) {
              # Get the column index of the matching question in state_survey_data
              question_index <- which(colnames(state_survey_data) == question_to_match)
              
              # Append the value to the current row (state) and matching column (question)
              state_survey_data[i, question_index] <- value_to_append
            }
          }
        }, error = function(e) {
          # Handle errors if any occur during the process (e.g., invalid link)
          message(paste("Error processing Excel link:", cell_value, e))
        })
        
      }
    }
  }
}


#### Organize and delete Cleaned Dataset ####
state_polls <- state_survey_data
# Delete totals of states
state_polls <- state_polls %>%
  select(-matches(".*Total$"))

# Change the value in cell 1 to "National"
state_polls[1, 1] <- "National"

# Delete columns with more than 10 NAs
state_polls_clean <- state_polls %>%
  select(where(~ sum(is.na(.)) <= 25))

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
cleaned_dataset <- state_polls_clean

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
                            stringsAsFactors = FALSE)

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

write_parquet(timeline_data, "data/02-analysis_data/timeline_data.parquet")
write_csv(timeline_data, "data/02-analysis_data/timeline_data.csv")