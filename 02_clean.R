# script purpose: 
# converts classes
# list them here

# setup =========
library(here)
library(tidyverse)
library(gsubfn)

source("01_import.R")

# Create time variable ==========

## Reformat dates and create time variable by subtracting visit date from previous visit date per participant
names(visits)[4] <- "day"
visits$day <- as.Date(visits$day, format = "%Y-%m-%d")

## Make "Time" variable by subtracting visit date from previous visit date per participant
visits <- visits %>%
  group_by(patient_id) %>% arrange(patient_id, visit) %>%
  mutate(time= day - first(day), 
         time = as.numeric(time))

# Tidy age variable ==========
#Change Age factor from years/months to just years
create_age <- function(x){
  years <- x$age_years * 12
  months <- x$age_months
  x <- x %>% 
    add_column(age = (years + months) / 12)
  x$age <- format(round(x$age, 2), nsmall = 2)
  x$age <- as.numeric(x$age)

  x
}
socio_demographics <- create_age(socio_demographics)

# Clean CSF variables =========

## Combine recalculated ABeta values and original values, change class to numeric
csf<- as.data.frame(lapply(csf, function (y) gsub("Recalculated result =", "", y)))
csf<- as.data.frame(lapply(csf, function (y) gsub(" PG/ML", "", y)))
## If result is ">1700", replace with re-calculated result
csf$abeta_1_42_result <- ifelse(csf$abeta_1_42_result == ">1700", csf$abeta_1_42_comments, csf$abeta_1_42_result)

csf <- csf%>%
mutate_at(vars(c("abeta_1_42_result",
                 "ptau_result",
                 "ttau_result")), as.numeric)

# APOE variable ==========
apoe <- apoe %>%
  mutate(apoe_e4 = case_when(
    apoe_result == "e4/e4" | apoe_result == "e2/e4" | apoe_result == "e3/e4" ~ "1",
    apoe_result == "e2/e3" | apoe_result == "e2/e2" | apoe_result == "e3/e3" ~ "0"))
