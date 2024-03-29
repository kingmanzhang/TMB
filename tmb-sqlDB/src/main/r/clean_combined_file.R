library(readxl)
library(tidyverse)

data <- read_excel("/Users/Aaron/git/TMB/tmb-sqlDB/src/main/r/tmb_melanoma/Melanoma_Combined_File_ad.xlsx", sheet = 1)

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
data <- data %>% mutate(`Stage at Presentation` = paste(`Tumor Stage`, `Stage at Presentation`,`Neoplasm Disease Stage American Joint Committee on Cancer Code`)) %>%
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

write_csv(patient_sample_cleaned, "/Users/Aaron/git/TMB/tmb-sqlDB/src/main/r/tmb_melanoma/patient_sample_cleaned.csv")


## continue cleaning individual columns
patient_sample_cleaned <- read.csv("/Users/Aaron/git/TMB/tmb-sqlDB/src/main/r/tmb_melanoma/patient_sample_cleaned.csv", stringsAsFactors = FALSE, header = TRUE)

# change cancer type details
patient_sample_cleaned$cancer_type.detailed[patient_sample_cleaned$cancer_type.detailed == 'Cutaneous Melanoma'] = 'Cutaneous Squamous Cell Carcinoma'
patient_sample_cleaned$cancer_type.detailed[patient_sample_cleaned$cancer_type.detailed == 'Cutaneous Melanom'] = 'Cutaneous Squamous Cell Carcinoma'


# change race
patient_sample_cleaned$race_category[patient_sample_cleaned$race_category == "WHITE"] <- "White"
patient_sample_cleaned$race_category[patient_sample_cleaned$race_category == "Casucasian"] <- "Caucasian"
patient_sample_cleaned$race_category[patient_sample_cleaned$race_category == "African American"] <- "Black or African American"
patient_sample_cleaned$race_category[patient_sample_cleaned$race_category == "African American(Haitan)"] <- "Black or African American"
patient_sample_cleaned$race_category[patient_sample_cleaned$race_category == "BLACK OR AFRICAN AMERICAN"] <- "Black or African American"
patient_sample_cleaned$race_category[patient_sample_cleaned$race_category == "ASIAN"] <- "Asian"

# immunotherapy
patient_sample_cleaned$immunotherapy <- if_else(patient_sample_cleaned$study_id == 'cscc_dfarber_2015', 'not reported', patient_sample_cleaned$immunotherapy)
patient_sample_cleaned$immunotherapy <- if_else(patient_sample_cleaned$study_id == 'cscc_hgsc_bcm_2014', 'not reported', patient_sample_cleaned$immunotherapy)
patient_sample_cleaned$immunotherapy <- if_else(patient_sample_cleaned$study_id == 'desm_broad_2015', 'not reported', patient_sample_cleaned$immunotherapy)

patient_sample_cleaned <- patient_sample_cleaned %>% 
    mutate(immunotherapy = if_else(str_trim(study_id) == 'mel_tsam_liang_2017', "not reported", immunotherapy))
patient_sample_cleaned <- patient_sample_cleaned %>%
    mutate(immunotherapy = if_else(str_trim(study_id) == 'mel_tsam_liang_2017' & 
                                       str_detect(str_to_lower(treatment), 'ipi'), 'ipilimumab', immunotherapy ))
patient_sample_cleaned <- patient_sample_cleaned %>%
    mutate(immunotherapy = if_else(str_trim(study_id) == 'mel_tsam_liang_2017' & 
                                       str_detect(str_to_lower(treatment), 'pem'), paste("pembrolizumab", immunotherapy), immunotherapy ))
patient_sample_cleaned <- patient_sample_cleaned %>%
    mutate(immunotherapy = if_else(str_trim(study_id) == 'mel_tsam_liang_2017' & 
                                       str_detect(str_to_lower(treatment), 'pd1'), paste('pd1', immunotherapy), immunotherapy ))
patient_sample_cleaned <- patient_sample_cleaned %>%
    mutate(immunotherapy = if_else(str_trim(study_id) == 'mel_tsam_liang_2017' & 
                                       str_detect(str_to_lower(treatment), 'no'), 'no', immunotherapy ))
        # todo: why some 'mel_tsam_liang_2017' still NA?

patient_sample_cleaned$immunotherapy <- if_else(patient_sample_cleaned$study_id == 'skcm_broad', 'not reported', patient_sample_cleaned$immunotherapy)

patient_sample_cleaned$immunotherapy <- if_else(patient_sample_cleaned$study_id == 'skcm_broad_brafresist_2012', 'not reported', patient_sample_cleaned$immunotherapy)

# skcm_broad_dfarber has treatment in data_clinical_sample, but not in the combined data
patient_id <- str_split("ME001,ME002,ME007,ME009,ME011,ME012,ME015,ME016,ME018,ME020,ME021,ME024,ME029,ME030,ME032,ME035,ME037,ME041,ME043,ME044,ME045,ME048,ME049,ME050,ME100,ME100", ',')[[1]]
sample_id <- str_split("ME001,ME002,ME007,ME009,ME011,ME012,ME015,ME016,ME018,ME020,ME021,ME024,ME029,ME030,ME032,ME035,ME037,ME041,ME043,ME044,ME045,ME048,ME049,ME050,ME100,ME100L", ',')[[1]]
treatment <- str_split("Interferon therapy, Chemotherapy|Interferon therapy, Chemotherapy, Immunotherapy, Vaccine|Interferon therapy, Chemotherapy, Immunotherapy, Vaccine, Radiation|Interferon therapy, Chemotherapy, Immunotherapy, Vaccine|Interferon therapy, Chemotherapy|Interferon therapy, Chemotherapy|Interferon therapy, Chemotherapy, Immunotherapy, Vaccine|None|Interferon therapy, Immunotherapy|None|Interferon therapy|Interferon therapy, Chemotherapy|Interferon therapy, Chemotherapy, Immunotherapy, Vaccine|None|Interferon therapy, Chemotherapy, Immunotherapy, Vaccine|Immunotherapy, Vaccine|None|None|Interferon therapy, Chemotherapy, Immunotherapy, Vaccine|None|Chemotherapy|Interferon therapy, Chemotherapy, Immunotherapy, Vaccine|Chemotherapy|Interferon therapy, Immunotherapy, Vaccine|None|None", '\\|')[[1]]
has_immunotherapy <- if_else(str_detect(treatment, 'Immunotherapy'), 'yes', 'no')
temp <- data.frame(study_id = rep('skcm_broad_dfarber', length(patient_id)), 
                   sample_id, 
                   has_immunotherapy, stringsAsFactors = FALSE)
patient_sample_cleaned <- patient_sample_cleaned %>% 
    left_join(temp, by =c("study_id", "sample_id")) %>% 
    mutate(immunotherapy = ifelse(study_id == 'skcm_broad_dfarber', has_immunotherapy, immunotherapy )) %>%
    select(-has_immunotherapy)

patient_sample_cleaned$immunotherapy <- if_else(patient_sample_cleaned$study_id == 'skcm_tcga_pan_can_atlas_2018', 'not reported', patient_sample_cleaned$immunotherapy)

patient_sample_cleaned$immunotherapy[patient_sample_cleaned$study_id=='skcm_ucla_2016'] <- 'pembrolizumab'
patient_sample_cleaned$immunotherapy[patient_sample_cleaned$study_id=='skcm_ucla_2016' & 
                                         is.element(patient_sample_cleaned$sample_id, c('Pt33', 'Pt36'))] <- 'no'

patient_sample_cleaned$immunotherapy <- if_else(patient_sample_cleaned$study_id == 'skcm_yale', 'not reported', patient_sample_cleaned$immunotherapy)

# patient_sample_cleaned %>% group_by(study_id, immunotherapy) %>% count() %>% View()

# change cancer stage
patient_sample_cleaned$stage_at.presentation <- gsub('NA', '', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('Stage', '', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('STAGE', '', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('C', '', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('B', '', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('A', '', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub(',', '', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('I/II', '1', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('\\(NOS\\)', '', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('Unknown', '', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('IV', '4', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('III', '3', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('II', '2', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('I', '1', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('NOS', '', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('M1c', '4', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('M1a', '4', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('M1b', '4', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('c', '', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('b', '', patient_sample_cleaned$stage_at.presentation)
patient_sample_cleaned$stage_at.presentation <- gsub('a', '', patient_sample_cleaned$stage_at.presentation)
# Stages are speicified based on the readings. I/II meaning it is stage 1 or 2. NOS means not otherwise specified. We chose to use stage 1 for all 1/2 and
# M1a/M1b/M1c are metastasis stages. At this stage, the cancer has spread to other parts of body so it is traditionally denoted as stage 4.


write_csv(patient_sample_cleaned, "/Users/Aaron/git/TMB/tmb-sqlDB/src/main/r/tmb_melanoma/patient_sample_cleaned.csv")

### continue
# change assay methods

data_laststudy <- read.csv(("/Users/Aaron/git/TMB/tmb-sqlDB/src/main/r/tmb_melanoma/dataset_assignment_sheet2.csv"), header = TRUE, sep = ",", stringsAsFactors = FALSE) 
data_laststudy <- data_laststudy %>% filter(study_id == 'desm_broad_2015')
patient_sample_cleaned <- patient_sample_cleaned %>% left_join(data_laststudy, by = c("study_id","patient_id", "sample_id"))
patient_sample_cleaned$assay <- if_else(patient_sample_cleaned$study_id=='desm_broad_2015', patient_sample_cleaned$assay.y, patient_sample_cleaned$assay.x)
patient_sample_cleaned$assay.x <- NULL
patient_sample_cleaned$assay.y <- NULL
patient_sample_cleaned$assay[patient_sample_cleaned$study_id == "skcm_ucla_2016"] <- "WES"
patient_sample_cleaned$assay[patient_sample_cleaned$study_id == "skcm_broad"] <- "WES"
patient_sample_cleaned$assay[patient_sample_cleaned$study_id == "skcm_tcga_pan_can_atlas_2018"] <- "WES"
patient_sample_cleaned$assay[patient_sample_cleaned$study_id == "skcm_broad_dfarber"] <- "WGS"
patient_sample_cleaned$assay[patient_sample_cleaned$study_id == "mel_tsam_liang_2017"] <- "WES"
patient_sample_cleaned$assay[patient_sample_cleaned$study_id == "cscc_hgsc_bcm_2014"] <- "WES"
patient_sample_cleaned$assay[patient_sample_cleaned$study_id == "skcm_yale"] <- "WES"
patient_sample_cleaned$assay[patient_sample_cleaned$study_id == "skcm_broad_brafresist_2012"] <- "WES"
patient_sample_cleaned$assay[patient_sample_cleaned$study_id == "cscc_dfarber_2015"] <- "OncoPanelv2"
patient_sample_cleaned$assay[patient_sample_cleaned$study_id == "skcm_vanderbilt_mskcc_2015"] <- "MSK-IMPACT"

patient_sample_cleaned$genome_covered_length <- 30
patient_sample_cleaned$genome_covered_length <- if_else(patient_sample_cleaned$study_id == 'cscc_dfarber_2015', 2.9, patient_sample_cleaned$genome_covered_length)
patient_sample_cleaned$genome_covered_length <- if_else(patient_sample_cleaned$study_id == 'desm_broad_2015' & patient_sample_cleaned$assay == 'WES', 35.7, patient_sample_cleaned$genome_covered_length)
patient_sample_cleaned$genome_covered_length <- if_else(patient_sample_cleaned$study_id == 'desm_broad_2015' & patient_sample_cleaned$assay == 'NimbleGen', 7, patient_sample_cleaned$genome_covered_length)
patient_sample_cleaned$genome_covered_length <- if_else(patient_sample_cleaned$study_id == 'skcm_vanderbilt_mskcc_2015', 1.5, patient_sample_cleaned$genome_covered_length)
patient_sample_cleaned$genome_covered_length <- if_else(patient_sample_cleaned$study_id == 'cscc_hgsc_bcm_2014', 42, patient_sample_cleaned$genome_covered_length)
patient_sample_cleaned$genome_covered_length <- if_else(patient_sample_cleaned$study_id == 'skcm_yale', 22.45, patient_sample_cleaned$genome_covered_length)

patient_sample_cleaned$mutation_count <- as.numeric(patient_sample_cleaned$mutation_count)
patient_sample_cleaned$normalised_mut_count <- round((patient_sample_cleaned$mutation_count/patient_sample_cleaned$genome_covered_length),2)
patient_sample_cleaned$normalised_mut_count <- if_else(is.element(patient_sample_cleaned$assay, c( 'WES', 'Exome', 'WGS')), 1.5 * patient_sample_cleaned$normalised_mut_count, patient_sample_cleaned$normalised_mut_count)

write_csv(patient_sample_cleaned, "/Users/Aaron/git/TMB/tmb-sqlDB/src/main/r/tmb_melanoma/patient_sample_cleaned.csv")

### add mutation count directly from our mutation database (excluding non-silent mutation)
patient_sample_cleaned <- read.csv("/Users/Aaron/git/TMB/tmb-sqlDB/src/main/r/tmb_melanoma/patient_sample_cleaned.csv", stringsAsFactors = FALSE)
db_url <- "/Users/Aaron/git/TMB/tmb-sqlDB/src/main/resources/tmb.sqlite"
dbcon <- dbConnect(RSQLite::SQLite(), db_url)
query = "SELECT Study_Id, Tumor_Sample_Barcode, count(*) as N FROM   (SELECT DISTINCT Study_Id, Tumor_Sample_Barcode, Hugo_Symbol, HGVSp_Short FROM mutations) as temp GROUP BY Study_Id, Tumor_Sample_Barcode ORDER BY Study_Id, Tumor_Sample_Barcode"
calculated_mut <- dbGetQuery(dbcon,query)
#note: the query count mutations from the raw mutations tables (mutations_mskcc and mutations_extended), but only picks unique mutations per gene
#it is under counting because in this case, homozygous will be counted once
calculated_mut <- calculated_mut %>% rename(study_id = Study_Id, sample_id = Tumor_Sample_Barcode, non_silent_mutation_count = N)

patient_sample_cleaned <- patient_sample_cleaned %>% left_join(calculated_mut, by = c('study_id', 'sample_id'))

patient_sample_cleaned <- patient_sample_cleaned %>% mutate(normalised_mut_count_non_silent = non_silent_mutation_count / genome_covered_length)

write_csv(patient_sample_cleaned, "/Users/Aaron/git/TMB/tmb-sqlDB/src/main/r/tmb_melanoma/patient_sample_cleaned.csv")
