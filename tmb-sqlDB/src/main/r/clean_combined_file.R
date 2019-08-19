library(readxl)
library(tidyverse)

data <- read_excel("/Users/Aaron/git/TMB/src/main/r/tmb_melanoma/Melanoma_Combined_File_ad.xlsx", sheet = 1)

# merge Column T (Age at Diagnosis) and Column CM (Diagnosis Age)
data$`Diagnosis Age` <- ifelse(is.na(data$`Diagnosis Age`), data$`Age at Diagnosis`, data$`Diagnosis Age`)
data$`Age at Diagnosis` <- NULL

# merge column U and AT
data <- data %>% 
    mutate(`Disease Free Status` = if_else(is.na(`Disease Free Status`), `Disease Status`, `Disease Free Status`)) %>%
    select(-`Disease Status`)

# merge column AA and AW
data <- data %>%
    mutate(`Progress Free Survival (Months)` = if_else(is.na(`Progress Free Survival (Months)`), `Event Free Survival (Months)`, `Progress Free Survival (Months)`)) %>%
    select(-`Event Free Survival (Months)`)

# merge AB, BU, BX, CN, DB, JO, paste them together
na_to_empty_str <- function(x){
    if_else(is.na(x) | x == "NA", "", x)
}
AB <- na_to_empty_str(data$`Radiation Therapy`)
BU <- na_to_empty_str(data$`Prior Treatment`)
BX <- na_to_empty_str(data$`Patient received Radiation or Chemotherapy`)
CN <- na_to_empty_str(data$`Other Alternative Therapy Given`)
DB <- na_to_empty_str(data$`Did patient start adjuvant postoperative radiotherapy?`)
temp <- data.frame(AB, BU, BX, CN, DB)
temp <- temp %>% mutate(`Radiation Therapy` = str_c(AB, BU, BX, CN, DB))
data$`Radiation Therapy` = temp$`Radiation Therapy`

#merge stages, DD, EB, EE
data <- data %>%
    mutate(`Stage at Presentation` = if_else(is.na(`Stage at Presentation`), `Tumor Stage`, `Stage at Presentation`)) %>%
    mutate(`Stage at Presentation` = if_else(is.na(`Stage at Presentation`), `Neoplasm Disease Stage American Joint Committee on Cancer Code`, `Stage at Presentation`)) %>%
    select(-c(`Tumor Stage`, `Neoplasm Disease Stage American Joint Committee on Cancer Code`))


#merge purity
data <- data %>% 
    mutate(`Tumor Purity` = if_else(is.na(`Tumor Purity`), Purity, `Tumor Purity`)) %>%
    select(-Purity)

#merge race and ethnicity
data <- data %>%
    mutate(`Race Category` = if_else(is.na(`Race Category`), `Ethnicity Category`, `Race Category`)) %>%
    select(-`Ethnicity Category`)

col_of_interest <- str_split("Study ID
                            Patient ID
                            Sample ID
                            Age (tumor resected)
                            Cancer Type
                            Cancer Type Detailed
                            Gene Panel
                            Mutation Count
                            Oncotree Code
                            Number of Samples Per Patient
                            Somatic Status
                            Vismo Status
                            Diagnosis Age
                            Disease Free Status
                            Overall Survival (Months)
                            Overall Survival Status
                            Other Sample ID
                            Progress Free Survival (Months)
                            Radiation Therapy
                            Sex
                            Smoker
                            Absolute Purity
                            Total Mutations
                            Anatomic Region
                            Disease Free (Months)
                            Follow up time months
                            Race Category
                            Stage at Presentation
                            Sub-site
                            Time to Recurrence (months)
                            Treatment
                            Age At Procurement
                            duration of therapy weeks
                            Early Resistance
                            Matched Normal Tissue
                            medication
                            Treatment best response
                            Braf Status
                            nras status
                            Tumor Purity
                            Days to Sample Collection.
                            days_to_patient_progression_free
                            days_to_tumor_progression
                            Fraction Genome Altered
                            ICD-10 Classification
                            Aneuploidy Score
                            Progression Free Status
                            RECIST Response
                            Chemotherapy
                            Immunotherapy
                            Response grade
                            Response to Target Therapy
                            Mutation Status", "\n( )+")[[1]]
patient_sample_cleaned <- data[, col_of_interest]
rename_col = colnames(patient_sample_cleaned)
new_col = str_to_lower(rename_col)
colnames(patient_sample_cleaned) <- str_replace(make.names(new_col), "\\.", "_")
patient_sample_cleaned <- patient_sample_cleaned %>% rename(assay = gene_panel)





write_csv(patient_sample_cleaned, "/Users/Aaron/git/TMB/src/main/r/tmb_melanoma/patient_sample_cleaned.csv", )



