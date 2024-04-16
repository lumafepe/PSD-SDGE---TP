package org.dht;

import io.grpc.ServerBuilder;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.dht.config.Manager;

import java.io.IOException;

public class Main {
    private static final Logger logger = LogManager.getLogger();
    private static final Manager configManager = Manager.getInstance();

    public static void main(String[] args) throws IOException, InterruptedException {
        logger.info("Starting DHT Node");

        try {
            configManager.load(args[0]);
        } catch(Exception e) {
            logger.fatal(e.getMessage());
            return;
        }

        ServerBuilder.forPort(configManager.getDHT().getPort())
                .addService(new DHTService())
                .build()
                .start()
                .awaitTermination();
        logger.info("Terminating DHT Node");
    }
}