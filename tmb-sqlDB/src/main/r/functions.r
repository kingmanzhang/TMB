library(tidyverse)

excel_col_to_row_str <- function(x) {
    str_c(str_split(x, '\n')[[1]], collapse="|")
}

# define function to chose the non-NA value of two vectors of the same length. Use reduce to pass a function to handle two non-NA values
non_na <- function(v1, v2, reduce=NA){
    assertthat::are_equal(length(v1), length(v2))
    if (is.na(reduce)){
        reduce = function(x, ...) {return (x)}
    }
    N = length(v1)
    v = vector(mode = "character", length = N)
    for (i in 1:N){
        if (!is.na(v1[i]) & is.na(v2[i])){
            v[i] = v1[i]
        } else if (is.na(v1[i]) & !is.na(v2[i])){
            v[i] = v2[i]
        } else if (!is.na(v1[i]) & !is.na(v2[i])){
            v[i] = do.call(reduce, list(v1[i], v2[i]))
        } else{
            v[i] = NA
        }
    }
    v
}

# define function to clean mutations for study 2
clean_mutation_2 <- function(mskcc_path, extended_path){
    mutations_mskcc = read.csv(mskcc_path, comment.char = "#", header = TRUE, sep = "\t")
    mutations_extended = read.csv(mskcc_path, comment.char = "#", header = TRUE, sep = "\t")
    columns_of_interest = c("Tumor_Sample_Barcode", "Hugo_Symbol", "Entrez_Gene_Id", "Chromosome", 
                            "Consequence", "Variant_Classification", "Variant_Type", "HGVSp_Short", 
                            "Protein_position", "Codons")
    cleaned <- mutations_mskcc[, columns_of_interest] %>% 
        union(mutations_extended[, columns_of_interest]) 
    cleaned$Amino_acid_mutation = str_extract(cleaned$HGVSp_Short, "[A-Z]\\d.+[A-Z]$")
    return (cleaned)
}

# test the above function
mskcc_path = "/Users/Aaron/Desktop/novartis_hackathon/hack_dataset_3/data_mutations_mskcc.txt"
extended_path = "/Users/Aaron/Desktop/novartis_hackathon/hack_dataset_3/data_mutations_extended.txt"
mutations_cleaned <- clean_mutation_2(mutations_mskcc_path, mutations_extended_path)
cleaned = clean_mutation_2(mskcc_path, extended_path)
head(cleaned)


#########################################################
# define function to clean mutations for study 3
clean_mutation_3 <- function(mskcc_path, extended_path){
    mutations_mskcc = read.csv(mskcc_path, comment.char = "#", header = TRUE, sep = "\t")
    mutations_extended = read.csv(mskcc_path, comment.char = "#", header = TRUE, sep = "\t")
    columns_of_interest = c("Tumor_Sample_Barcode", "Hugo_Symbol", "Entrez_Gene_Id", "Chromosome", 
                            "Consequence", "Variant_Classification", "Variant_Type", "HGVSp_Short", 
                            "Protein_position", "Codons")
    cleaned <- mutations_mskcc[, columns_of_interest] %>% 
        union(mutations_extended[, columns_of_interest]) 
    cleaned$Amino_acid_mutation = str_extract(cleaned$HGVSp_Short, "[A-Z]\\d.+[A-Z]$")
    return (cleaned)
}

mskcc_path = "/Users/Aaron/Desktop/novartis_hackathon/hack_dataset_4/data_mutations_mskcc.txt"
extended_path = "/Users/Aaron/Desktop/novartis_hackathon/hack_dataset_4/data_mutations_extended.txt"
cleaned = clean_mutation_3(mskcc_path, extended_path)
head(cleaned)
# test the above function


#########################################################
# define function to clean mutations for study 4
clean_mutation_4 <- function(mskcc_path, extended_path){
    
}

# test the above function


#########################################################
# define function to clean mutations for study 5
clean_mutation_5 <- function(mskcc_path, extended_path){
    
}

# test the above function


#########################################################
# define function to clean mutations for study 6
clean_mutation_6 <- function(mskcc_path, extended_path){
    
}

# test the above function


#########################################################
# define function to clean mutations for study 7
clean_mutation_7 <- function(mskcc_path, extended_path){
    
}

# test the above function


#########################################################
# define function to clean mutations for study 8
clean_mutation_8 <- function(mskcc_path, extended_path){
    
}

mskcc_path = "/Users/Aaron/Desktop/novartis_hackathon/hack_dataset_8/data_mutations_mskcc.txt"
extended_path = "/Users/Aaron/Desktop/novartis_hackathon/hack_dataset_8/data_mutations_extended.txt"
mutations_mskcc = read.csv(mskcc_path, comment.char = "#", header = TRUE, sep = "\t")
mutations_extended = read.csv(mskcc_path, comment.char = "#", header = TRUE, sep = "\t")

# test the above function


#########################################################
# define function to clean mutations for study 9
clean_mutation_9 <- function(mskcc_path, extended_path){
    mutations_mskcc = read.csv(mskcc_path, comment.char = "#", header = TRUE, sep = "\t")
    mutations_extended = read.csv(mskcc_path, comment.char = "#", header = TRUE, sep = "\t")
    columns_of_interest = c("Tumor_Sample_Barcode", "Hugo_Symbol", "Entrez_Gene_Id", "Chromosome", 
                            "Consequence", "Variant_Classification", "Variant_Type", "HGVSp_Short", 
                            "Protein_position", "Codons")
    cleaned <- mutations_mskcc[, columns_of_interest] %>% 
        union(mutations_extended[, columns_of_interest]) 
    cleaned$Amino_acid_mutation = str_extract(cleaned$HGVSp_Short, "[A-Z]\\d.+[A-Z]$")
    return (cleaned)
}

mskcc_path = "/Users/Aaron/Desktop/novartis_hackathon/hack_dataset_9/data_mutations_mskcc.txt"
extended_path = "/Users/Aaron/Desktop/novartis_hackathon/hack_dataset_9/data_mutations_extended.txt"
mutations_mskcc = read.csv(mskcc_path, comment.char = "#", header = TRUE, sep = "\t")
mutations_extended = read.csv(mskcc_path, comment.char = "#", header = TRUE, sep = "\t")
cleaned = clean_mutation_9(mskcc_path, extended_path)
head(cleaned)

# test the above function


#########################################################
# define function to clean mutations for study 10
clean_mutation_10 <- function(mskcc_path, extended_path){
    
}

# test the above function


#########################################################
# define function to clean mutations for study 11
clean_mutation_11 <- function(mskcc_path, extended_path){
    
}

# test the above function


#########################################################
# define function to clean mutations for study 12
clean_mutation_12 <- function(mskcc_path, extended_path){
    
}

# test the above function


#########################################################
# define function to clean mutations for study 13
clean_mutation_13 <- function(mskcc_path, extended_path){
    
}

# test the above function


#########################################################
    
