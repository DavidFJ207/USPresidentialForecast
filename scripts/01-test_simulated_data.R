#### Preamble ####
# Purpose: Tests the structure and validity of the American Election
# Author: Gadiel David Flores
# Date: October 29, 2024
# Contact: davidgadiel.flores@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `tidyverse` package must be installed and loaded
# - 00-simulate_data.R must have been run

#### Workspace setup ####
library(tidyverse)

sim_data <- read_csv(here::here("data/00-simulated_data/simulated_poll_data.csv"))

# Test if the data was successfully loaded
if (exists("sim_data")) {
  message("Test Passed: The dataset was successfully loaded.")
} else {
  stop("Test Failed: The dataset could not be loaded.")
}

#### Test data ####
# Check if all column headers in 'sim_data' are unique
if (length(unique(names(sim_data))) == length(names(sim_data))) {
  message("Test Passed: All column headers in 'sim_data' are unique.")
} else {
  stop("Test Failed: There are duplicate column headers in 'sim_data'.")
}

# Check if the 'state' column contains only valid American state names
valid_states <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", 
  "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", 
  "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
  "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
  "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
  "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
  "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
  "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
  "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
  "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", 
  "National"
)


if (all(sim_data$State %in% valid_states)) {
  message("Test Passed: The 'state' column contains only valid American state names.")
} else {
  stop("Test Failed: The 'state' column contains invalid state names.")
}

# Check if there are any missing values in the dataset
if (all(!is.na(sim_data))) {
  message("Test Passed: The dataset contains no missing values.")
} else {
  stop("Test Failed: The dataset contains missing values.")
}

# Check if the dataset has the expected number of columns (e.g., at least 5)
if (ncol(sim_data) >= 5) {
  message("Test Passed: The dataset has at least 5 columns.")
} else {
  stop("Test Failed: The dataset has fewer than 5 columns.")
}

# Check if all columns in the dataset contain at least one non-NA value
if (all(sapply(sim_data, function(x) any(!is.na(x))))) {
  message("Test Passed: All columns contain at least one non-NA value.")
} else {
  stop("Test Failed: One or more columns are entirely NA.")
}

# Check if all columns have consistent data types (e.g., no mixed types in the same column)
if (all(sapply(sim_data, function(x) length(unique(sapply(x, class))) == 1))) {
  message("Test Passed: All columns have consistent data types.")
} else {
  stop("Test Failed: One or more columns have mixed data types.")
}

# Check if the dataset has at least one row
if (nrow(sim_data) > 0) {
  message("Test Passed: The dataset has at least one row.")
} else {
  stop("Test Failed: The dataset is empty.")
}

# Check if the data contains reasonable numeric values (e.g., no negative percentages)
if (all(sapply(sim_data, function(x) if (is.numeric(x)) all(x >= 0, na.rm = TRUE) else TRUE))) {
  message("Test Passed: All numeric columns contain non-negative values.")
} else {
  stop("Test Failed: One or more numeric columns contain negative values.")
}

# Check if there are any duplicate rows in the dataset
if (nrow(sim_data) == nrow(distinct(sim_data))) {
  message("Test Passed: There are no duplicate rows in the dataset.")
} else {
  stop("Test Failed: There are duplicate rows in the dataset.")
}
