#clear the work space
rm(list=ls()) #clear all variables

library(dplyr)
# set working directory
setwd("~/Desktop/Hackathon2019/TMB")

# load first data file in CSV
Cleaned_data= read.csv("patient_sample_cleaned.csv", header = TRUE)

# View(Cleaned_data)

library(dplyr)
Cleaned_data_test <- mutate(Cleaned_data, Diagnose_age = paste(age_.tumor.resected., diagnosis_age)) 

# View(Cleaned_data_test)

# Convert Roman letter to number for the cancer grade
Cleaned_data$stage_at.presentation <- gsub('III', '3', Cleaned_data$stage_at.presentation)
Cleaned_data$stage_at.presentation <- gsub('IV', '4', Cleaned_data$stage_at.presentation)
Cleaned_data$stage_at.presentation <- gsub('II', '2', Cleaned_data$stage_at.presentation)
Cleaned_data$stage_at.presentation <- gsub('I', '1', Cleaned_data$stage_at.presentation)
Cleaned_data$stage_at.presentation <- gsub('a', 'A', Cleaned_data$stage_at.presentation)
Cleaned_data$stage_at.presentation <- gsub('NA', '', Cleaned_data$stage_at.presentation)
Cleaned_data$stage_at.presentation <- gsub('c', 'C', Cleaned_data$stage_at.presentation)
Cleaned_data$stage_at.presentation <- gsub('b', 'B', Cleaned_data$stage_at.presentation)



# Cleaned_data_test=Cleaned_data_test[is.na(x = Cleaned_data_test)] <- ""
Age_column= subset(Cleaned_data_test, select = c(Diagnose_age) )