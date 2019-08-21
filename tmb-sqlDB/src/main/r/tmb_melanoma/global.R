patient_sample_cleaned <- read.csv("patient_sample_cleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ',')
patient_sample_cleaned$stage_at.presentation <- as.factor(patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$assay <- if_else(patient_sample_cleaned$assay == 'Exome', 'WES', patient_sample_cleaned$assay)
