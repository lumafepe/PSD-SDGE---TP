package org.dht;

import io.grpc.ServerBuilder;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.dht.config.ConfigManager;
import org.dht.exceptions.InvalidConfigurationFileException;
import picocli.CommandLine;

import java.io.IOException;

@CommandLine.Command(
        name = "DHT Server",
        mixinStandardHelpOptions = true,
        version = "1.0",
        description = "Starts the DHT Node server"
)
public class Main implements Runnable {
    private static final Logger logger = LogManager.getLogger();
    private static final ConfigManager configManager = ConfigManager.getInstance();

    @CommandLine.Option(
            names = {"-c", "--config"},
            required = true,
            description = "Path to configuration YAML file"
    )
    private String configFile;

    private static void runServer() throws IOException, InterruptedException {
        MetadataManager manager = new MetadataManager();

        Thread entryThread = new Thread(new StartupHandler(manager));
        entryThread.start();

        ServerBuilder.forPort(configManager.getConfig().getDht().getPort())
                .addService(new DHTService(manager))
                .build()
                .start()
                .awaitTermination();

        entryThread.wait();
    }

    public void run()  {
        logger.info("Starting DHT Node");

        try {
            configManager.load(this.configFile);
            runServer();
        } catch(IOException | InvalidConfigurationFileException | InterruptedException e) {
            logger.fatal(e.getMessage());
            return;
        }

        logger.info("Terminating DHT Node");
    }

    public static void main(String[] args) {
        new CommandLine(new Main()).execute(args);
    }
}