# script purpose: 
# Create time and apoe variables
# Combine original and re-calculated A-beta values
# Merge dataframes with variables of interest into one 


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
         time = as.numeric(time),
         time_months = time / 30)

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

# Merge ==========
df1 <- merge(csf, socio_demographics, by="patient_id") #not the most elegant way of merging, but other methods run into memory issues
df2 <- left_join(df1, unique(visits))
df3 <- left_join(df2, unique(family_history))
df4 <- left_join(df3, unique(cdr))
df5 <- left_join(df4, unique(eligibility))
df6 <- left_join(df5, unique(dementia_diag))
df7 <- left_join(df6, unique(mmse))
df8 <- left_join(df7, unique(rbans))
epad_merged <- left_join(df8, unique(apoe))
