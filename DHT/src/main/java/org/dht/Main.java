package org.dht;

import io.grpc.ServerBuilder;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.dht.config.Manager;

import java.io.IOException;

public class Main {
    private static final Logger logger = LogManager.getLogger();
    private static final Manager configManager = Manager.getInstance();

    private static void runServer() throws IOException, InterruptedException {
        MetadataManager manager = new MetadataManager();

        Thread entryThread = new Thread(new StartupHandler(manager));
        entryThread.start();

        ServerBuilder.forPort(configManager.getDHT().getPort())
                .addService(new DHTService(manager))
                .build()
                .start()
                .awaitTermination();

        entryThread.wait();
    }

    public static void main(String[] args) throws IOException, InterruptedException {
        logger.info("Starting DHT Node");

        try {
            configManager.load(args[0]);
        } catch(Exception e) {
            logger.fatal(e.getMessage());
            return;
        }

        runServer();

        logger.info("Terminating DHT Node");
    }
}