package com.kingmanzhang.Command;

import com.beust.jcommander.Parameter;
import com.beust.jcommander.Parameters;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.BatchPreparedStatementSetter;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallbackWithoutResult;
import org.springframework.transaction.support.TransactionTemplate;

import java.io.*;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Parameters(commandDescription = "load mutations into database" )
public class LoadMutationCmd implements TmbCmd {
    @Parameter(names = "-i", description = "dir of studies to analyze, separate by ,")
    //it should be the dir names of studies, separated by comma
    private String paths;
    @Parameter(names = {"-s", "--study"}, description =
            "corresponding study name(s)")
    private String studyIds;
    @Autowired
    JdbcTemplate jdbcTemplate;
    @Autowired
    TransactionTemplate transactionTemplate;

    @Override
    public void run() {
        String [] dirs = paths.split(",");
        String [] study_ids = studyIds.split(",");
        if (dirs.length != study_ids.length){
            System.err.println("count of study names is not equal to " +
                    "directories");
            System.exit(1);
        }
        for (int i = 0; i < dirs.length; i++){
            String study_id = study_ids[i];
            String dir = dirs[i];
            String mutation_mskcc = dir + File.separator + "data_mutations_mskcc.txt";
            String mutation_extended = dir + File.separator + "data_mutations_extended.txt";
            load(study_id, mutation_mskcc);
            load(study_id, mutation_extended);
        }
    }

    public void initTable(){
        String query = "CREATE TABLE IF NOT EXISTS mutations (Study_Id VARCHAR(255), Tumor_Sample_Barcode VARCHAR(255), Hugo_Symbol VARCHAR(255),Entrez_Gene_Id VARCHAR(255), Chromosome VARCHAR(255), Consequence VARCHAR(255), Variant_Classification VARCHAR(255), Variant_Type VARCHAR(255), HGVSp_Short VARCHAR(255));";
        jdbcTemplate.execute(query);
    }

    private Map<String, Integer> initColumnNameMap(String mutation_file){
        Map<String, Integer> columnMap = new HashMap<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(mutation_file))){
            String line;
            while ((line = reader.readLine()) != null){
                if(line.contains("Hugo_Symbol")){
                    String[] colnames = line.split("\t");
                    for (int i=0; i < colnames.length; i++){
                        columnMap.put(colnames[i], i);
                    }
                    break;
                }
            }

        } catch (FileNotFoundException e){

        } catch (IOException e){

        }
        return columnMap;
    }

    public void load(String study_id, String mutation_file){
        initTable();

        Map<String, Integer> columnMap = initColumnNameMap(mutation_file);

        if (columnMap.size() == 0){
            return;
        }

        int i_Tumor_Sample_Barcode = columnMap.get("Tumor_Sample_Barcode");
        int i_Hugo_Symbol = columnMap.get("Hugo_Symbol");
        int i_Entrez_Gene_Id = columnMap.get("Entrez_Gene_Id");
        int i_Chromosome = columnMap.get("Chromosome");
        int i_Consequence = columnMap.get("Consequence");
        int i_Variant_Classification = columnMap.get("Variant_Classification");
        int i_Variant_Type = columnMap.get("Variant_Type");
        int i_HGVSp_Short = columnMap.get("HGVSp_Short");

        try(BufferedReader reader = new BufferedReader(new FileReader(mutation_file))){
            String line;
            List<Mutation> batch = new ArrayList<>();
            while ((line = reader.readLine()) != null){
                if (line.startsWith("#")){
                    continue;
                }
                if (line.contains("Hugo_Symbol")){
                    continue;
                }
                String [] elements = line.split("\t");

                String Tumor_Sample_Barcode = elements[i_Tumor_Sample_Barcode];
                String Hugo_Symbol = elements[i_Hugo_Symbol];
                String Entrez_Gene_Id = elements[i_Entrez_Gene_Id];
                String Chromosome = elements[i_Chromosome];
                String Consequence = elements[i_Consequence];
                String Variant_Classification = elements[i_Variant_Classification];
                String Variant_Type = elements[i_Variant_Type];
                String HGVSp_Short = elements[i_HGVSp_Short];

                if (Variant_Classification.toLowerCase().contains("silent")){
                    continue;
                }

                Mutation mutation = new Mutation(study_id,
                        Tumor_Sample_Barcode,
                        Hugo_Symbol,
                        Entrez_Gene_Id,
                        Chromosome,
                        Consequence,
                        Variant_Classification,
                        Variant_Type,
                        HGVSp_Short);
                batch.add(mutation);
                if (batch.size() > 1000){
                    loadBatch(batch);
                    batch.clear();
                }
            }
            if (!batch.isEmpty()){
                loadBatch(batch);
            }
        } catch (FileNotFoundException e){
            e.printStackTrace();
        } catch (IOException e){
            e.printStackTrace();
        }


    }

    private void loadBatch(List<Mutation> mutations){

        String sql = "INSERT INTO mutations " + "(Study_Id, Tumor_Sample_Barcode, Hugo_Symbol, Entrez_Gene_Id, Chromosome, Consequence, Variant_Classification, Variant_Type, HGVSp_Short) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)";

        transactionTemplate.execute(new TransactionCallbackWithoutResult() {

            @Override
            protected void doInTransactionWithoutResult(TransactionStatus status) {
                jdbcTemplate.batchUpdate(sql, new BatchPreparedStatementSetter() {

                    @Override
                    public void setValues(PreparedStatement ps, int i) throws SQLException {
                        Mutation mutation = mutations.get(i);
                        ps.setString(1, mutation.getStudy_Id());
                        ps.setString(2, mutation.getTumor_Sample_Barcode());
                        ps.setString(3, mutation.getHugo_Symbol());
                        ps.setString(4, mutation.getEntrez_Gene_Id());
                        ps.setString(5, mutation.getChromosome());
                        ps.setString(6, mutation.getConsequence());
                        ps.setString(7, mutation.getVariant_Classification());
                        ps.setString(8, mutation.getVariant_Type());
                        ps.setString(9, mutation.getHGVSp_Short());
                    }

                    @Override
                    public int getBatchSize() {
                        return mutations.size();
                    }
                });
            }
        });
    }

    private static class Mutation{
        private String Study_Id;
        private String Tumor_Sample_Barcode;
        private String Hugo_Symbol;
        private String Entrez_Gene_Id;
        private String Chromosome;
        private String Consequence;
        private String variant_Classification;
        private String Variant_Type;
        private String HGVSp_Short;

        public Mutation(String study_Id, String tumor_Sample_Barcode, String hugo_Symbol, String entrez_Gene_Id, String chromosome, String consequence, String variant_Classification, String variant_Type, String HGVSp_Short) {
            Study_Id = study_Id;
            Tumor_Sample_Barcode = tumor_Sample_Barcode;
            Hugo_Symbol = hugo_Symbol;
            Entrez_Gene_Id = entrez_Gene_Id;
            Chromosome = chromosome;
            Consequence = consequence;
            this.variant_Classification = variant_Classification;
            Variant_Type = variant_Type;
            this.HGVSp_Short = HGVSp_Short;
        }

        public String getStudy_Id() {
            return Study_Id;
        }

        public void setStudy_Id(String study_Id) {
            Study_Id = study_Id;
        }

        public String getTumor_Sample_Barcode() {
            return Tumor_Sample_Barcode;
        }

        public void setTumor_Sample_Barcode(String tumor_Sample_Barcode) {
            Tumor_Sample_Barcode = tumor_Sample_Barcode;
        }

        public String getHugo_Symbol() {
            return Hugo_Symbol;
        }

        public void setHugo_Symbol(String hugo_Symbol) {
            Hugo_Symbol = hugo_Symbol;
        }

        public String getEntrez_Gene_Id() {
            return Entrez_Gene_Id;
        }

        public void setEntrez_Gene_Id(String entrez_Gene_Id) {
            Entrez_Gene_Id = entrez_Gene_Id;
        }

        public String getChromosome() {
            return Chromosome;
        }

        public void setChromosome(String chromosome) {
            Chromosome = chromosome;
        }

        public String getConsequence() {
            return Consequence;
        }

        public void setConsequence(String consequence) {
            Consequence = consequence;
        }

        public String getVariant_Classification() {
            return variant_Classification;
        }

        public void setVariant_Classification(String variant_Classification) {
            this.variant_Classification = variant_Classification;
        }

        public String getVariant_Type() {
            return Variant_Type;
        }

        public void setVariant_Type(String variant_Type) {
            Variant_Type = variant_Type;
        }

        public String getHGVSp_Short() {
            return HGVSp_Short;
        }

        public void setHGVSp_Short(String HGVSp_Short) {
            this.HGVSp_Short = HGVSp_Short;
        }
    }
}
