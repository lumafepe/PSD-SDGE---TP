package org.dht;

import io.grpc.ServerBuilder;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.util.List;

public class Main {
    private static final Logger logger = LogManager.getLogger();
    private static final ConfigManager configManager = ConfigManager.getInstance();

    private static void runServer() throws IOException, InterruptedException {
        ServerBuilder.forPort(configManager.getPort())
                .addService(new DHTService())
                .build()
                .start()
                .awaitTermination();
    }

    public static void main(String[] args) throws IOException, InterruptedException {
        logger.info("Starting DHT Node");

        try {
            configManager.parseConfig(args);
        } catch(IllegalArgumentException e) {
            logger.fatal(e.getMessage());
            return;
        }

        runServer();

        logger.info("Terminating DHT Node");
    }
}