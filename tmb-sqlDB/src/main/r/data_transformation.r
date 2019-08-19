library(tidyverse)
hack1_data3_patient <- read.csv('/Users/Aaron/Desktop/novartis_hackathon/hack_data_3/data_clinical_patient.txt', comment.char = '#', header = TRUE, sep= '\t')
hack1_data3_patient_cleaned <- hack1_data3_patient %>% 
    select(PATIENT_ID, AGE, SEX) %>% 
    rename(patient_id = PATIENT_ID, age = AGE, sex = SEX)
write.csv(hack1_data3_patient_cleaned, file = '/Users/Aaron/Desktop/novartis_hackathon/preprocessed/hack1_data3_patient.csv', row.names = FALSE)


hack1_data3_sample <- read.csv('/Users/Aaron/Desktop/novartis_hackathon/hack_data_3/data_clinical_sample.txt', comment.char = '#', header = TRUE, sep= '\t')

head(hack1_data3_sample)
sample_part1 <- hack1_data3_sample %>% 
    select(PATIENT_ID, SAMPLE_ID, ONCOTREE_CODE) %>%
    rename(patient_id = PATIENT_ID, sample_id = SAMPLE_ID, oncotree_code = ONCOTREE_CODE )
head(sample_part1)
sample_part2 <- hack1_data3_patient %>% 
    select(PATIENT_ID, TOTAL_MUTATIONS, OS_STATUS, TREATMENT_TYPE, N_STAGE, M_STAGE, )
sample_part2

hack1_data3_sample_cleaned