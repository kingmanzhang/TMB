package com.kingmanzhang;

import javax.sql.DataSource;

import com.kingmanzhang.Command.LoadMutationCmd;
import com.kingmanzhang.Command.LoadPatientSampleCmd;
import com.kingmanzhang.Command.TmbCmd;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.jdbc.DataSourceBuilder;
import org.springframework.context.EnvironmentAware;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

@Configuration
public class AppConfig {

    @Autowired
    Environment env;

    @Bean
    public String dbPath(Environment env){
        return env.getProperty("spring.jpa.dabase.path");
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
