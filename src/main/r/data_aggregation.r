library(tidyverse)
library(data.table)

# aggregate patient and clinical samples
# strategy: find the union of column names of all datasets
# then combine them together

# define function to bind all columns of two dataframes. Fill in with NA if column is missing from one dataframe.
rbind.all.columns <- function(x, y) {
    
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    x[, c(as.character(y.diff))] <- NA
    y[, c(as.character(x.diff))] <- NA
    
    return(rbind(x, y))
}
FILE_SEP = '/'
parent_dir = '/Users/Aaron/Desktop/novartis_hackathon'
setwd(parent_dir)
patient_sample <- data.frame(study=NA)
mutations <- data.frame(study=NA)
for (i in 2:13) {
    dataset_dir = paste('hack_dataset_', i, sep="")
    if (dir.exists(dataset_dir)){
        data_clinical_patient_path = paste(parent_dir, dataset_dir, 'data_clinical_patient.txt', sep = FILE_SEP)
        data_clinical_sample_path = paste(parent_dir, dataset_dir, 'data_clinical_sample.txt', sep = FILE_SEP)
        data_mutations_mskcc_path = paste(parent_dir, dataset_dir, 'data_mutations_mskcc.txt', sep = FILE_SEP)
        data_mutations_extended_path = paste(parent_dir, dataset_dir, 'data_mutations_extended.txt', sep = FILE_SEP)
        data_clinical_patient <- read.csv(data_clinical_patient_path, comment.char = '#', sep = '\t', header = TRUE)
        data_clinical_sample <- read.csv(data_clinical_sample_path, comment.char = '#', sep = '\t', header = TRUE)
        data_mutations_mskcc <- read.csv(data_mutations_mskcc_path, comment.char = '#', sep = '\t', header = TRUE)
        data_mutations_extended <- read.csv(data_mutations_extended_path, comment.char = '#', sep = '\t', header = TRUE)
        patient_sample_join <- data_clinical_patient %>%
            left_join(data_clinical_sample, by = c('PATIENT_ID'))
        patient_sample_join$study = i
        patient_sample = rbind.all.columns(patient_sample, patient_sample_join)
        
        mutations_union = data_mutations_mskcc %>%
            union(data_mutations_extended)
        mutations_union$study = i
        mutations = rbind.all.columns(mutations, mutations_union)
    }
}

#look at the mutation data for each dataset, and find animo acid changes for each gene
#provide the data and callback, return a cleaned mutation table
clean_mutations <- function(data, callback){
    do.call(callback, data)
}



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

#patient_sample_mutations <- patient_sample_join %>%
#    left_join(mutations_union, by = )

data_clinical_patient_subset <- data_clinical_patient 
#data_clinical_sample_subset <- data_clinical_sample
#data_mutations_mskcc_subset <- data_mutations_mskcc
#data_mutations_extended <- data_mutations_extended

