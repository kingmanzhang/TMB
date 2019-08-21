# Query for a single gene, binarize the result to wildtype (WT) or mutant (MT). Return a dataframe for plotting
# tumor mutation counts against the gene mutation status.
query_samples_with_mutant_gene <- function(gene_symbol, dbcon){
    query <-  sprintf("
          SELECT 
                DISTINCT Study_Id, Tumor_Sample_Barcode, Hugo_Symbol, HGVSp_Short 
          FROM 
                mutations 
          WHERE Hugo_Symbol = '%s'", gene_symbol)
    result <- dbGetQuery(dbcon, query)
    return (result)
}

query_samples_with_mutant_gene_within_studies <- function(gene_symbol, dbcon, studies){
    studies_formatted <- str_c(shQuote(studies), collapse = ",")
    query <-  sprintf("
          SELECT 
                DISTINCT Study_Id, Tumor_Sample_Barcode, Hugo_Symbol, HGVSp_Short 
          FROM 
                mutations 
          WHERE Hugo_Symbol = '%s' AND study_id IN (%s)", gene_symbol, studies_formatted)
    result <- dbGetQuery(dbcon, query)
    return (result)
}

query_samples_with_mutant_gene_at_position <- function(gene_symbol, position, dbcon){
    query <-  sprintf("
          SELECT 
                DISTINCT Study_Id, Tumor_Sample_Barcode, Hugo_Symbol, HGVSp_Short 
          FROM 
                mutations 
          WHERE Hugo_Symbol = '%s' AND HGVSp_Short LIKE 'p._%d_'", gene_symbol, position)
    dbGetQuery(dbcon, query)
}

query_samples_with_mutant_gene_at_position_within_studies <- function(gene_symbol, position, dbcon, studies){
    studies_formatted <- str_c(shQuote(studies), collapse = ",")
    query <-  sprintf("
          SELECT 
                DISTINCT Study_Id, Tumor_Sample_Barcode, Hugo_Symbol, HGVSp_Short 
          FROM 
                mutations 
          WHERE Hugo_Symbol = '%s' AND HGVSp_Short LIKE 'p._%d_' AND study_id IN (%s)", gene_symbol, position, studies_formatted)
    dbGetQuery(dbcon, query)
}


tmb_vs_single_gene <- function(gene_symbol, dbcon, patient_sample_cleaned){
    query <-  sprintf("
          SELECT 
                DISTINCT Study_Id, Tumor_Sample_Barcode, Hugo_Symbol, HGVSp_Short 
          FROM 
                mutations 
          WHERE Hugo_Symbol = '%s'", gene_symbol)
    target_samples <- dbGetQuery(dbcon, query)
    data_for_plot <- patient_sample_cleaned %>% 
        select(study_id, patient_id, sample_id, normalised_mut_count, normalised_mut_count_non_silent) %>% 
        left_join(
            target_samples %>% 
                rename(study_id = Study_Id, sample_id = Tumor_Sample_Barcode) %>%
                mutate(mutant = 'MT'), 
            by = c('study_id', 'sample_id')
        ) %>% 
        mutate(HGVSp_Short = if_else(is.na(mutant), 'WT', mutant))
    return (data_for_plot)
}

# Query for a gene amino acid, which could be mutated to multiple forms (excluding silent mutation). Return a dataframe 
#for plotting tumor mutation counts against the gene mutation status at the specified position.
tmb_vs_single_gene_at_position <- function(gene_symbol, position, dbcon, patient_sample_cleaned){
    query <-  sprintf("
          SELECT 
                DISTINCT Study_Id, Tumor_Sample_Barcode, Hugo_Symbol, HGVSp_Short 
          FROM 
                mutations 
          WHERE Hugo_Symbol = '%s' AND HGVSp_Short LIKE 'p._%d_'", gene_symbol, position)
    target_samples <- dbGetQuery(dbcon, query)
    data_for_plot <- patient_sample_cleaned %>% 
        select(study_id, patient_id, sample_id, normalised_mut_count, normalised_mut_count_non_silent) %>% 
        left_join(
            target_samples %>% 
                rename(study_id = Study_Id, sample_id = Tumor_Sample_Barcode) %>%
                mutate(mutant = 'YES'),
            by = c('study_id', 'sample_id')
        ) %>% 
        mutate(HGVSp_Short = if_else(is.na(mutant), 'WT_aa', HGVSp_Short))
    return (data_for_plot)
}

# Query for BRAF mutations at V600. Return a dataframe for plotting tmb against V600E, V600K, V600 others, and WT.
# TODO: figure out exactly what mutations status are required
braf_v600 <- function(dbcon, patient_sample_cleaned){
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
    
    data_for_plot <- patient_sample_cleaned %>% select(study_id, patient_id, sample_id, normalised_mut_count, normalised_mut_count_non_silent) %>% 
        left_join(braf_c, by = c('study_id', 'sample_id'))
    return (data_for_plot)
}

p_value_matrix <- function(list, test='wilcox.test'){
    N = length(list)
    m <- matrix(' ', nrow = N, ncol = N)
    for (i in 1:N){
        for (j in 1:N){
            symbol = ' '
            p <- wilcox.test(list[[i]], list[[j]])$p.value
            if (p < 0.05) {
                symbol = '*'
            } else if (p < 0.01){
                symbol = '**'
            } else if (p < 0.001){
                symbol = '*'
            } else {
                symbol = ' '
            }
            m[i,j] = symbol
        }
    }
    return (m)
}
