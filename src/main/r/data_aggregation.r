library(tidyverse)

FILE_SEP = '/'
parent_dir = '/Users/Aaron/Desktop/novartis_hackathon'
dataset_dir = 'hack_data_3'
data_clinical_patient_path = paste(parent_dir, dataset_dir, 'data_clinical_patient.txt', sep = FILE_SEP)
data_clinical_sample_path = paste(parent_dir, dataset_dir, 'data_clinical_sample.txt', sep = FILE_SEP)
data_mutations_mskcc_path = paste(parent_dir, dataset_dir, 'data_mutations_mskcc.txt', sep = FILE_SEP)
data_mutations_extended_path = paste(parent_dir, dataset_dir, 'data_mutations_extended.txt', sep = FILE_SEP)
data_clinical_patient <- read.csv(data_clinical_patient_path, comment.char = '#', sep = '\t', header = TRUE)
data_clinical_sample <- read.csv(data_clinical_sample_path, comment.char = '#', sep = '\t', header = TRUE)
data_mutations_mskcc <- read.csv(data_mutations_mskcc_path, comment.char = '#', sep = '\t', header = TRUE)
data_mutations_extended <- read.csv(data_mutations_extended_path, comment.char = '#', sep = '\t', header = TRUE)

mutations_union = data_mutations_mskcc %>%
    union(data_mutations_extended)

patient_sample_join <- data_clinical_patient %>%
    left_join(data_clinical_sample, by = c('PATIENT_ID'))

patient_sample_mutations <- patient_sample_join %>%
    left_join(mutations_union, by = )
