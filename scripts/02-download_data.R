#### Preamble ####
# Purpose: Downloads and saves the presidential general election polls data 
# from FiveThirtyEight
# Author: Tina Kim
# Date: 7 October 2024 
# Contact: tinak.kim@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)


#### Download data ####
raw_president_polls_data <-
  read_csv(
    file = 
      "https://projects.fivethirtyeight.com/polls/data/president_polls.csv",
    show_col_types = FALSE,
  )

write_csv(
  x = raw_president_polls_data,
  file = "president_polls.csv"
)


#### Save data ####
write_csv(raw_president_polls_data, "data/01-raw_data/raw_data.csv")
