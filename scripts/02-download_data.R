#### Preamble ####
# Purpose: Downloads and saves the data from [...UPDATE THIS...]
# Author: Tina Kim
# Date: 7 October 2024 
# Contact: tinak.kim@mail.utoronto.ca
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


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
# change the_raw_data to whatever name you assigned when you downloaded it.
write_csv(raw_president_polls_data, "data/01-raw_data/raw_data.csv") 

         
