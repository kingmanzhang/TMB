##########################
#
#create a function can get gene related tables
#
#
#input: 
#@dataset: dbcon 
#@clincial_dataset: the combined clinical dataset with TMB in github
#@gene1/gene2/gene3: genes of interest
#
#output:
#A table with 7 columns
#@sample_id @GENE1 @GENE2 @GENE3
#@study_id @patient_id @normalised_mut_count 
#
##########################


combine_genes <- function(dataset,clincial_dataset,gene1,gene2,gene3) {
  # select dataset from db
  query1 <- sprintf("
                   SELECT DISTINCT Study_Id, Tumor_Sample_Barcode, Hugo_Symbol
                   FROM mutations
                   WHERE Hugo_Symbol = '%s'",gene1
  );
  dataset1 <- dbGetQuery(dataset,query1);
  query2 <- sprintf("
                   SELECT DISTINCT Study_Id, Tumor_Sample_Barcode, Hugo_Symbol
                   FROM mutations
                   WHERE Hugo_Symbol = '%s'",gene2
  );
  dataset2 <- dbGetQuery(dataset,query2);
  query3 <- sprintf("
                   SELECT DISTINCT Study_Id, Tumor_Sample_Barcode, Hugo_Symbol
                   FROM mutations
                   WHERE Hugo_Symbol = '%s'",gene3
  );
  dataset3 <- dbGetQuery(dataset,query3);
  
  # combine gene1,gene2,gene3
   dataset_gene_interest12 <- bind_rows(dataset1,dataset2);
   dataset_gene_interest <- bind_rows(dataset_gene_interest12,dataset3);
   names(dataset_gene_interest) <- c("study_id","sample_id","hugo_symbol")
   
  # create new gene dataset
   new_dataset <- dataset_gene_interest %>%
     mutate(gene1 = 0) %>%
     mutate(gene2 = 0) %>%
     mutate(gene3 = 0) ;
   
   # add value
   new_dataset$"gene1"[new_dataset$hugo_symbol==gene1] = 1;
   new_dataset$"gene2"[new_dataset$hugo_symbol==gene2] = 1;
   new_dataset$"gene3"[new_dataset$hugo_symbol==gene3] = 1;
   
   # remove hugo_symbol
   new_dataset <- new_dataset[-c(3)];
   
   # sum it up to get sum(mutated genes)
   sum_data_gene1 <- new_dataset %>%
     group_by(sample_id) %>%
     summarise(GENE1 = sum(gene1));
   sum_data_gene2 <- new_dataset %>%
     group_by(sample_id) %>%
     summarise(GENE2 = sum(gene2));
   sum_data_gene3 <- new_dataset %>%
     group_by(sample_id) %>%
     summarise(GENE3 = sum(gene3));
   data_gene23 <- full_join(sum_data_gene2,sum_data_gene3,by="sample_id");
   data_gene123 <- full_join(sum_data_gene1,data_gene23,by="sample_id");   

   # normalize the mutation count
   data_gene123$"GENE1"[data_gene123$"GENE1">0] = 1;
   data_gene123$"GENE2"[data_gene123$"GENE2">0] = 1;
   data_gene123$"GENE3"[data_gene123$"GENE3">0] = 1;  
   # combine clinical file
   final_data <- left_join(data_gene123,data_patient_TMB,by=c("sample_id")) ;
   
   return(final_data)
}







  




