package com.kingmanzhang;

import javax.sql.DataSource;

import com.kingmanzhang.Command.LoadMutationCmd;
import com.kingmanzhang.Command.LoadPatientSampleCmd;
import com.kingmanzhang.Command.TmbCmd;
import org.springframework.boot.jdbc.DataSourceBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class AppConfig {
    @Bean
    public DataSource dataSource() {
        DataSourceBuilder dataSourceBuilder = DataSourceBuilder.create();
        dataSourceBuilder.driverClassName("org.sqlite.JDBC");
        dataSourceBuilder.url("jdbc:sqlite:src/main/resources/tmb.sqlite");
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
