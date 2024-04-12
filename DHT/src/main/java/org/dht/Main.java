package org.dht;

import io.grpc.ServerBuilder;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;

public class Main {
    private static final Logger logger = LogManager.getLogger();
    public static void main(String[] args) throws IOException, InterruptedException {
        logger.info("Starting DHT Node");
        ServerBuilder.forPort(4200)
                .addService(new DHTService(new FileMap("/home/ruioliveira02/Documents/Projetos/PSD-SDGE---TP/DHT/test_data")))
                .build()
                .start()
                .awaitTermination();
        logger.info("Terminating DHT Node");
    }
}