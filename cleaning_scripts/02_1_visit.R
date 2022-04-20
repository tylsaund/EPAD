# script purpose: cleaning Visits dataframe script. Only necessary if using longitudinal data.

# setup ==========
library(tidyverse)
library(here) # for hiding path names

source("01_import.R")

# Create time variable =====

# reformat "Date" as date type
visits$visdat_int <- as.Date(visits$visdat_int, format = "%Y-%m-%d")
# make "Time" variable by subtracting visit date from previous visit date per participant
visits <- visits %>%
  group_by(patient_id) %>% arrange(patient_id, visit) %>%
  mutate(time= visdat_int - first(visdat_int))
# remove "days" from time variable 
visits$time <- gsub(" days","", visits$time) %>%
  as.numeric()
