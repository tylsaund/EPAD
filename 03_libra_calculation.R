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

## Cardiac Condition, diabetes, renal dysfunction, alcohol use, physical activity, smoking, depression
cardiac_conditions <- c("Myocardial infarction", "Myocardial ischaemia", 
                        "Atrial fibrillation", "Angina pectoris", "Cardiac failure", 
                        "Coronary artery disease")

renal_conditions <- c("Nephrolithiasis", "Renal colic", "Renal cyst", "Chronic kidney disease", "Nephrotic syndrome", "Single functional kidney")


libra_data<- libra_data %>%
  mutate(libra_cardiac = ifelse(libra_data$patient_id %in% medical_history$patient_id[medical_history$preferred_term %in% cardiac_conditions],1,0),
         libra_diabetes = ifelse(libra_data$patient_id %in% medical_history$patient_id[medical_history$preferred_term == "Diabetes mellitus"], 1.3, 0),
         libra_renal = ifelse(libra_data$patient_id %in% medical_history$patient_id[medical_history$preferred_term %in% renal_conditions], 1.1, 0),
         libra_alcohol = ifelse(libra_data$lcs22 == "1-2" | libra_data$lcs22 == "3-4" | libra_data$lcs22 == "5-6 (or about 1 unit per day)" | libra_data$lcs22 == "7-14 (or about 1-2 units per day)", -1.0, 0),
         libra_exercise = ifelse(libra_data$physical_activity == "Not at all" | libra_data$physical_activity == "A few times a year" | libra_data$physical_activity == "2-3 times a month" | libra_data$physical_activity == "Once a week", 1.1, 0),
         libra_smoking = ifelse(libra_data$smoking == "Current", 1.5, 0),
         libra_depression = ifelse(libra_data$gds_total > 10, 2.1, 0),
         )
         
#Hypertension (medical history or blood-pressure based definition)         
libra_data <- libra_data %>%
  mutate(libra_hyperten = case_when(
    libra_data$patient_id %in% medical_history$patient_id[medical_history$preferred_term == "Hypertension"] ~ 1.6,
    libra_data$systolic_bp > 140 ~ 1.6,
    libra_data$diastolic_bp > 90 ~ 1.6,
    libra_data$systolic_bp > 140 & libra_data$diastolic_bp > 90 ~ 1.6,
    TRUE ~ 0
  ))

#Hypercholesterolaemia (medical history or lipid-modifying medications definition)
libra_data <- libra_data %>%
  mutate(libra_cholesterol = case_when(
    libra_data$patient_id %in% medical_history$patient_id[medical_history$preferred_term == "Hypercholesterolaemia" | medical_history$preferred_term == "Dyslipidaemia"] ~ 1.4,
    libra_data$patient_id %in% current_medication$patient_id[substr(current_medication$atc_code,1,4) == "C10A"] ~ 1.4,
    TRUE ~ 0
  ))

#Obesity 
libra_data <- libra_data %>%
  mutate(bmi = libra_data$weight / ((libra_data$height/100)^2)) %>% 
  mutate(libra_obesity = case_when(
    libra_data$bmi >= 30 ~ 1.6,
    libra_data$sex == "m" & libra_data$waist_circumference > 102 ~ 1.6,
    libra_data$sex == "f" & libra_data$waist_circumference > 88 ~ 1.6,
    TRUE ~ 0
  ))
           
#MEDAS diet score
libra_data$lcs2_4[libra_data$lcs2_4 >=150] <- NA #scores over 150 are errors so recoded as NA
libra_data <- libra_data %>%
  mutate(libra_medas = )

rule1 <- ifelse((LIBRA_Final$lcs2_2+LIBRA_Final$lcs2_4)<7, 1, 0) 
table(is.na(rule1)) #32 missing

