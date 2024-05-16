package org.client;

import org.cli.Root;
import org.cli.Sender;


public class Main {
    public static void main(String[] args) {

        Sender router = Sender.getInstance();

        router.setIdentity(args[0]);
        router.setAddress("localhost");
        router.setPort(4321);

        router.setup();

        Root root = new Root();
        root.execute("");
    }
}
