# Script purpose: 
## Compute LIBRA score

# Import ==========
demograph <- read.csv(here("data","raw", "v_imi_epadlcs_socio_demographics.csv"), header=TRUE)
vitals <- read.csv(here("data","raw", "v_imi_epadlcs_vital_signs.csv"), header = TRUE)
gds <- read.csv(here("data","raw", "v_imi_epadlcs_gds.csv"), header= TRUE)
life <- read.csv(here("data","raw", "v_imi_epadlcs_life.csv"), header = TRUE)
hatice <- read.csv(here("data","raw", "v_imi_epadlcs_hatice.csv"), header = TRUE)


demograph <- demograph[c(1:10)]
gds <- select(gds, patient_id, visit, gds_total)
life <- select(life, patient_id, visit, physical.activity, smoking, drug_abuse, drug_name)

#Subset data for visit 1 only probably a function can do this
vitals_v1 <- subset(vitals, visit == "V1")
gds_v1 <- subset(gds, visit =='V1') #Subset of baseline data
life_v1 <- subset(life_v1, visit == "V1")
hatice_v1 <- subset(hatice_v1, visit == "V1")

Hatice_v1 <- subset(Hatice, visit =='V1') #Subset of baseline data
table(Hatice_v1$visit) #Double check vist types in subset
