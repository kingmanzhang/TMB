# plot against gene types

# load patient*sample data
patient_sample_cleaned <- read.csv("/Users/Aaron/git/TMB/tmb-sqlDB/src/main/r/tmb_melanoma/patient_sample_cleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ',')

# connect to mutations database
library(RSQLite)
library(tidyverse)

db_url <- "/Users/Aaron/git/TMB/tmb-sqlDB/src/main/resources/tmb.sqlite"
dbcon <- dbConnect(RSQLite::SQLite(), db_url)

test<- dbGetQuery(dbcon, "SELECT * FROM mutations LIMIT 5")

query <- "SELECT 
                DISTINCT Study_Id, Tumor_Sample_Barcode, Hugo_Symbol, HGVSp_Short 
          FROM 
                mutations 
          WHERE Hugo_Symbol = 'BRAF' AND HGVSp_Short LIKE 'p.V600E'"
braf_V600E <- dbGetQuery(dbcon, query)

query <- "SELECT 
                DISTINCT Study_Id, Tumor_Sample_Barcode, Hugo_Symbol, HGVSp_Short 
          FROM 
                mutations 
          WHERE Hugo_Symbol = 'BRAF' AND HGVSp_Short LIKE 'p.V600K'"
braf_V600K <- dbGetQuery(dbcon, query)

query <- "SELECT 
                DISTINCT Study_Id, Tumor_Sample_Barcode, Hugo_Symbol, HGVSp_Short 
          FROM 
                mutations 
          WHERE Hugo_Symbol = 'BRAF' AND HGVSp_Short LIKE 'p.V600_' AND HGVSp_Short NOT LIKE 'p.V600K' AND HGVSp_Short NOT LIKE 'p.V600E'"
braf_V600other <- dbGetQuery(dbcon, query)

braf_c <- patient_sample_cleaned %>% 
    select(study_id, patient_id, sample_id) %>%
    mutate(BRAF_V600 = rep('WT', nrow(patient_sample_cleaned))) %>%
    left_join(
        braf_V600E %>% 
            rename(study_id = Study_Id, sample_id = Tumor_Sample_Barcode) %>%
            mutate(V600E = 'YES') %>%
            select(study_id, sample_id, V600E),
        by = c('study_id', 'sample_id')
    ) %>% 
    mutate(BRAF_V600 = if_else(is.na(V600E), BRAF_V600,  'V600E')) 

braf_c <- braf_c %>%
    left_join(
        braf_V600K %>% 
            rename(study_id = Study_Id, sample_id = Tumor_Sample_Barcode) %>%
            mutate(V600K = 'YES') %>%
            select(study_id, sample_id, V600K),
        by = c('study_id', 'sample_id')
        ) %>% 
    mutate(BRAF_V600 = if_else(is.na(V600K), BRAF_V600,  'V600K'))

braf_c <- braf_c %>%
    mutate(BRAF_V600 = if_else(!is.na(V600E) & !is.na(V600K), 'V600E V600K', BRAF_V600))

braf_c <- braf_c %>%
    left_join(
        braf_V600other %>% 
            rename(study_id = Study_Id, sample_id = Tumor_Sample_Barcode) %>%
            mutate(V600other = 'YES') %>%
            select(study_id, sample_id, V600other),
        by = c('study_id', 'sample_id')
    ) %>% 
    mutate(BRAF_V600 = if_else(is.na(V600other), BRAF_V600,  'V600other'))

data_for_plot <- patient_sample_cleaned %>% select(study_id, patient_id, sample_id, normalised_mut_count) %>% 
    left_join(braf_c, by = c('study_id', 'sample_id'))

ggplot(data_for_plot) + geom_violin(aes(x = BRAF_V600, y = normalised_mut_count, fill = BRAF_V600)) + 
    xlab("genotype") + ylab('tumor mutation burden') +
    scale_y_continuous(limits = c(0, 100))+
    theme_bw() + theme(panel.grid = element_blank(), legend.position = 'na')

# extend to any genes, binarize to WT or MT
gene = 'NRAS'
query <-  sprintf("
          SELECT 
                DISTINCT Study_Id, Tumor_Sample_Barcode, Hugo_Symbol, HGVSp_Short 
          FROM 
                mutations 
          WHERE Hugo_Symbol = '%s'", gene)
target_samples <- dbGetQuery(dbcon, query)
data_for_plot <- patient_sample_cleaned %>% 
    select(study_id, patient_id, sample_id, normalised_mut_count) %>% 
    left_join(
        target_samples %>% 
            rename(study_id = Study_Id, sample_id = Tumor_Sample_Barcode) %>%
            mutate(mutant = 'MT'), 
        by = c('study_id', 'sample_id')
    ) %>% 
    mutate(HGVSp_Short = if_else(is.na(mutant), 'WT', mutant))

ggplot(data_for_plot) + 
    geom_violin(aes(x = HGVSp_Short, y = normalised_mut_count, fill = HGVSp_Short )) +
    xlab(paste(gene, 'status'))


# extend to any genes, let user choose amino acid position
gene = 'BRAF'
amino_acid_position = 600   # should check within range
query <-  sprintf("
          SELECT 
                DISTINCT Study_Id, Tumor_Sample_Barcode, Hugo_Symbol, HGVSp_Short 
          FROM 
                mutations 
          WHERE Hugo_Symbol = '%s' AND HGVSp_Short LIKE 'p._%d_'", gene, amino_acid_position)
target_samples <- dbGetQuery(dbcon, query)
data_for_plot <- patient_sample_cleaned %>% 
    select(study_id, patient_id, sample_id, normalised_mut_count) %>% 
    left_join(
        target_samples %>% 
            rename(study_id = Study_Id, sample_id = Tumor_Sample_Barcode) %>%
            mutate(mutant = 'YES'),
        by = c('study_id', 'sample_id')
    ) %>% 
    mutate(HGVSp_Short = if_else(is.na(mutant), 'WT_aa', HGVSp_Short))

ggplot(data_for_plot) + 
    geom_violin(aes(x = HGVSp_Short, y = normalised_mut_count, fill = HGVSp_Short )) +
    xlab(sprintf("%s mutation status at position %d", gene, amino_acid_position))

# count the number of mutations per sample, excluding silent mutations
query = "SELECT Study_Id, Tumor_Sample_Barcode, count(*) as N FROM   (SELECT DISTINCT Study_Id, Tumor_Sample_Barcode, Hugo_Symbol, HGVSp_Short FROM mutations) as temp GROUP BY Study_Id, Tumor_Sample_Barcode ORDER BY Study_Id, Tumor_Sample_Barcode"
dbGetQuery(dbcon, query)
