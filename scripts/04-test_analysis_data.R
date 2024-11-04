#### Preamble ####
# Purpose: Testing cleaned pollster data for the 2024 US presidential election,
#          ensuring data quality and consistency with expected formats
# Author: Gadiel David Flores
# Date: October 29, 2024
# Contact: davidgadiel.flores@mail.utoronto.ca
# License: MIT
# 
# Pre-requisites:
# - The `tidyverse`, `lubridate`, and `testthat` packages must be installed and loaded.
# - Ensure that the following scripts have been run in the specified order:
#   1. 02-download_data.R
#   2. 03-1-clean_data.R
#   3. 03-2-clean-links-state-data.R
#   4. 03-3-api_extract_clean_data.R

#### Workspace setup ####
library(tidyverse)
library(testthat)
library(lubridate)

# Load the analysis data
analysis_data <- read_csv(here::here("data/02-analysis_data/timeline_data.csv"))

# Test Data Structure

test_that("Data has correct structure and identifiers", {
  # Test for required columns
  expected_cols <- c("state", "party", "start_date", "pct", "end_date")
  expect_true(all(expected_cols %in% colnames(analysis_data)), 
              info = "Missing one or more expected columns.")
  
})

# Test Data Types and Formats

test_that("Variables have correct data types", {
  # Test numeric variables
  numeric_vars <- c("pct")
  for (var in numeric_vars) {
    expect_true(is.numeric(analysis_data[[var]]), 
                info = paste(var, "should be numeric"))
  }
  
  # Test character variables
  char_vars <- c("state", "party")
  for (var in char_vars) {
    expect_true(is.character(analysis_data[[var]]), 
                info = paste(var, "should be character"))
  }
  
  # Test date variables
  expect_true(inherits(analysis_data$end_date, "Date"), 
              info = "'end_date' should be of class 'Date'")
})

# Test Value Ranges and Categories

test_that("Variables have valid values and categories", {
  # Test valid state names
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
  expect_true(all(analysis_data$state %in% valid_states), 
              info = "Invalid state names found in 'state' column.")
  
  # Test valid party names
  valid_parties <- c("DEM", "REP")
  expect_true(all(analysis_data$party %in% valid_parties), 
              info = "Invalid party names found in 'party' column.")
  
  # Test percentage values
  expect_true(all(analysis_data$pct >= 0 & analysis_data$pct <= 100), 
              info = "Percentage values in 'pct' should be between 0 and 100.")

})

# Test Data Consistency

test_that("Data shows internal consistency", {
  # Ensure there are no empty strings in 'state', or 'party'
  expect_false(any(analysis_data$state == "" | analysis_data$party == ""), 
               info = "Empty strings found in 'state', 'party'")
  
  # Ensure at least two unique party values are present
  expect_true(length(unique(analysis_data$party)) >= 2, 
              info = "There should be at least two unique party values.")
})

# Test Temporal Aspects

test_that("Dates are logical and within expected range", {
  # Test date range
  expect_true(all(analysis_data$end_date >= as.Date("2020-01-01")), 
              info = "Dates should not be earlier than January 1, 2020.")
  expect_true(all(analysis_data$end_date <= as.Date("2024-11-05")), 
              info = "Dates should not be later than November 5, 2024.")
})

# Test Data Quality

test_that("Data quality meets requirements", {
  # Check for missing values in critical columns
  critical_cols <- c("poll_id", "state", "party", "pct", "end_date")
  for (col in critical_cols) {
    expect_false(any(is.na(analysis_data[[col]])), 
                 info = paste("Missing values found in", col, "column."))
  }
  
})

# Test Statistical Properties

test_that("Statistical properties are reasonable", {
  # Check that 'pct' values are distributed within expected ranges
  state_means <- analysis_data %>%
    group_by(state) %>%
    summarise(mean_pct = mean(pct, na.rm = TRUE))
  expect_true(all(state_means$mean_pct >= 30 & state_means$mean_pct <= 70), 
              info = "Mean percentage values should be between 30 and 70.")
  
})
