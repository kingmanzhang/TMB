package com.kingmanzhang.tmb.sqlDB;

import com.beust.jcommander.JCommander;
import com.beust.jcommander.ParameterException;
import com.kingmanzhang.tmb.sqlDB.Command.TmbCmd;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

@Component
public class CmdRunner implements CommandLineRunner {

    @Autowired
    @Qualifier("loadMutationCmd")
    TmbCmd loadMutationCmd;
    @Autowired @Qualifier("loadPatientSampleCmd")
    TmbCmd loadPatientSampleCmd;

    @Override
    public void run(String... args) throws Exception {

        long startTime = System.currentTimeMillis();

        JCommander jc = JCommander.newBuilder()
                .addObject(this)
                .addCommand("loadMutation", loadMutationCmd)
                .addCommand("loadPatientSample", loadPatientSampleCmd)
                .build();

        try {
            jc.parse(args);
        } catch (ParameterException e) {
            for (String arg : args) {
                if (arg.contains("h")) {
                    jc.usage();
                    System.exit(0);
                }
            }
            e.printStackTrace();
            jc.usage();
            System.exit(1);
        }

        String command = jc.getParsedCommand();

        if (command == null) {
            jc.usage();
            System.exit(1);
        }

        TmbCmd cmd = null;
        System.out.println("Starting LoadMutationCmd " + command);

        switch (command) {
            case "loadMutation":
                cmd = loadMutationCmd;
                break;
            case "loadPatientSample":
                cmd = loadPatientSampleCmd;
                break;
            default:
                System.err.println(String.format("[ERROR] LoadMutationCmd \"%s\" not recognized",command));
                jc.usage();
                System.exit(1);
        }

        cmd.run();

        long stopTime = System.currentTimeMillis();
        System.out.println("TmbApp: Elapsed time was " + (stopTime - startTime)*(1.0)/1000 + " seconds.");

    }
}
