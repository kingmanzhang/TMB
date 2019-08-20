package com.kingmanzhang.tmb.sqlDB.Command;

import com.beust.jcommander.Parameter;
import com.beust.jcommander.Parameters;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.transaction.support.TransactionTemplate;


@Parameters(commandDescription = "load patient and samples into database")
public class LoadPatientSampleCmd implements TmbCmd {

    @Parameter(names = {"-i", "--input"}, description = "dir of studies to analyze, separate by ,")
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

    }
}
