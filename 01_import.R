# script purpose:
# - reads in all .csv files in a specified folder

# setup ==========
library(here) # path names
library(tidyverse) # general data wrangling and visualisation
library(fs) # file system operations

# import =========

## specify path to look for data
epad_data_path <- here("data","raw")
## list all the csv files stored in the folder
epad_csv_files <- dir_ls(epad_data_path, regexp = "\\.csv$")
# read in all csv files stored in the folder
epad_data_list <- epad_csv_files %>%
  map(~ read_csv(
    .,
    col_names = TRUE,
    trim_ws = TRUE,
    name_repair = "unique"
  ))
