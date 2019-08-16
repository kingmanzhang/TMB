#clear the work space
rm(list=ls()) #clear all variables

library(dplyr)
# set working directory
setwd("~/Desktop/Hackathon2019/skcm_broad_brafresist_2012")

# load first data file in CSV
SCM_broad_patient= read.csv("data_clinical_patient.csv", header = TRUE)

# inspect the data type
# cat("data_clinical_patient.csv: ",class(SCM_broad), "\n\n")

# delete columns
SCM_broad_patient = subset(SCM_broad_patient, select = -c(duration.of.therapy.weeks) )
# delete the non-useful rows
SCM_broad_patient[-c(1,2,3,4), ]
# reassign the data
SCM_broad_patient <- SCM_broad_patient[-c(1,2,3,4), ]

# load second data file 
SCM_broad_clinical_sample= read.csv("data_clinical_sample.csv", header = TRUE)
# delete columns
SCM_broad_clinical_sample = subset(SCM_broad_clinical_sample, select = -c(Cancer.Type,Cancer.Type.Detailed) )
# delete the non-useful rows
SCM_broad_clinical_sample[-c(1,2,3,4), ]
# reassign the data
SCM_broad_clinical_sample <- SCM_broad_clinical_sample[-c(1,2,3,4), ]


SCM_mutations_extended <- read.csv(file="data_mutations_extended.csv")


SCM_mutations_extended = subset(SCM_mutations_extended,select = c(Tumor_Sample_Barcode,Hugo_Symbol, Entrez_Gene_Id,Protein_position,Codons,Codon_Change,Protein_Change,SwissProt_entry_Id))

colnames(SCM_mutations_extended)[1] <- "Sample.Identifier"
# merging data
test_merge = merge(SCM_broad_clinical_sample,SCM_mutations_extended)

test_merge2 = merge(test_merge,SCM_broad_patient)

test_merge2 = subset(test_merge2, select = -c(Matched.Normal.Tissue,mean.target.coverage.normal,mean.target.coverage.tumor,SwissProt_entry_Id, Early.Resistance) )

SCM_broad_cleaned=test_merge2;

# write.csv(SCM_broad_cleaned,file = file.choose(new = T))
write.csv(SCM_broad_cleaned,'SCM_broad_cleaned.csv')