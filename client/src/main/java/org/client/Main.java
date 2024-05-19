package org.client;

import org.cli.Root;
import org.cli.Sender;
import java.util.Map;


public class Main {
    public static void main(String[] args) {
        Sender router = Sender.getInstance();

        String identity,address;
        identity = args[0];

        address = "localhost";

        router.setIdentity(identity);
        router.setAddress(address);
        router.setPort(4321);

        router.setup();

        Root root = new Root();
        root.execute("");
    }
}
