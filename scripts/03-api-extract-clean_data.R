#### Preamble ####
# Purpose: Extracts data from links into a summarize dataset
# an analysis dataset
# Author: Gadiel David Flores
# Date: 7 October 2024
# Contact: davidgadiel.flores@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need to cleaned poll data
# Any other information needed? Intall googledrive and googleAuthR

# Authenticate your Google account
gs4_auth()

#### Load Libraries ####
library(googledrive)
library(readxl)
library(tidyverse)
library(dplyr)

#### Helper Functions ####

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


#### Main Function to Process Google Sheet ####
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

#### State Dataset ####
state_data <- read_csv("data/02-analysis_data/states_surveys_links_data.csv")
mega_dataset <- list()

# Function to check if a string is a Google Sheets link
is_google_sheet_link <- function(text) {
  grepl("docs.google.com/spreadsheets", text)
}

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

# View the final mega dataset
print(mega_dataset_combined)

# Optionally, save the final dataset to a CSV file
write_csv(mega_dataset_combined, "data/02-analysis_data/mega_dataset.csv")

unique_items_column1 <- unique(mega_dataset_combined[[1]])
