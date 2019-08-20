package com.kingmanzhang.tmb.sqlDB;

import javax.sql.DataSource;

import com.kingmanzhang.tmb.sqlDB.Command.LoadMutationCmd;
import com.kingmanzhang.tmb.sqlDB.Command.LoadPatientSampleCmd;
import com.kingmanzhang.tmb.sqlDB.Command.TmbCmd;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.jdbc.DataSourceBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

@Configuration
public class AppConfig {

    @Autowired
    Environment env;

    @Bean
    public String dbPath(Environment env){
        return env.getProperty("spring.jpa.database.path");
    }

    @Bean
    public DataSource dataSource(String dbPath) {
        DataSourceBuilder dataSourceBuilder = DataSourceBuilder.create();
        dataSourceBuilder.driverClassName("org.sqlite.JDBC");
        dataSourceBuilder.url("jdbc:sqlite:" + dbPath);
        return dataSourceBuilder.build();
    }

    @Bean(name = "loadMutationCmd")
    public TmbCmd loadMutationCmd(){
        return new LoadMutationCmd();
    }

    @Bean(name = "loadPatientSampleCmd")
    public TmbCmd loadPatientSampleCmd(){
        return new LoadPatientSampleCmd();
    }
}
