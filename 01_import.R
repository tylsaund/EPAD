# script purpose:
# - reads in all .csv files in a specified folder

# setup ==========
library(here) # path names
library(tidyverse) # general data wrangling and visualisation
library(fs) # file system operations
library(janitor)
library(purrr)
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

# clean ====================

# remove the pathname, duplicate prefix, and filetype from the names of the list elements
epad_data_names <- epad_data_list %>%
  names() %>%
  str_remove_all(., paste0(epad_data_path)) %>%
  str_remove_all(., "/") %>%
  str_remove(., ".csv") %>%
  str_remove(., "v_imi_epadlcs_")

# set the new names
names(epad_data_list) <- epad_data_names

# change patient_id variable to character class across all dataframes
patientid_class <- function(df){
  within(df, patient_id <- as.character(patient_id))
}

epad_data_list<- lapply(epad_data_list, patientid_class)

# optional: check classes of patient_id (primary key to merge by) column_classes <- compare_df_cols(epad_data_list)

# expand dataframe list to environment as individual dataframes
list2env(epad_data_list, envir=.GlobalEnv)

