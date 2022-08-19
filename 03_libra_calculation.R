# Script purpose: 
## Compute LIBRA score

source("01_import.R")
source("02_clean.R")

epad_data <- read_rds(here("data","processed","epad_merged.RDS"))

#Select relevant variables to reduce data frame
libra_data <- subset(epad_data, visit == "V1")

#Remove participants not eligible 
libra_data <- subset(libra_data, dementia_diagnosed == "N")
libra_data <- subset(libra_data, cdr_global_score <= 0.5)
libra_data <- subset(libra_data, mmse_total >= 20)

# Calculate LIBRA score

## Cardiac Condition, diabetes, 
cardiac_conditions <- c("Myocardial infarction", "Myocardial ischaemia", 
                        "Atrial fibrillation", "Angina pectoris", "Cardiac failure", 
                        "Coronary artery disease")

libra_data<- libra_data %>%
  mutate(libra_cardiac = ifelse(libra_data$patient_id %in% medical_history$patient_id[medical_history$preferred_term %in% cardiac_conditions],1,0),
         libra_diabetes = ifelse(libra_data$patient_id %in% medical_history$patient_id[medical_history$preferred_term == "Diabetes mellitus"], 1.3, 0))
         
#Hypertension (medical history or blood-pressure based definition)         
libra_data <- libra_data %>%
  mutate(libra_hyperten = case_when(
    libra_data$patient_id %in% medical_history$patient_id[medical_history$preferred_term == "Hypertension"] ~ 1.6,
    libra_data$systolic_bp > 140 ~ 1.6,
    libra_data$diastolic_bp > 90 ~ 1.6,
    libra_data$systolic_bp > 140 & libra_data$diastolic_bp > 90 ~ 1.6,
    TRUE ~ 0
  ))