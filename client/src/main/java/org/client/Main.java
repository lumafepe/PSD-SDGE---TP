package org.client;

import org.cli.Root;
import org.cli.Sender;


public class Main {
    public static void main(String[] args) {

        Sender router = Sender.getInstance();

        router.setIdentity(String.valueOf(System.currentTimeMillis()));
        router.setAddress("localhost");
        router.setPort(9090);

        router.setup();

        Root root = new Root();
        root.execute("");
    }
}
