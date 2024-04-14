package org.dht;

import io.grpc.ServerBuilder;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;

public class Main {
    private static final Logger logger = LogManager.getLogger();
    private static final ConfigManager configManager = ConfigManager.getInstance();

    public static void main(String[] args) throws IOException, InterruptedException {
        logger.info("Starting DHT Node");

        try {
            configManager.parseConfig(args);
        } catch(IllegalArgumentException e) {
            logger.fatal(e.getMessage());
            return;
        }

        ServerBuilder.forPort(configManager.getPort())
                .addService(new DHTService())
                .build()
                .start()
                .awaitTermination();
        logger.info("Terminating DHT Node");
    }
}