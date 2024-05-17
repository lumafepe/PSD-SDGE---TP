package org.client;

import org.cli.Root;
import org.cli.Sender;
import java.util.Map;


public class Main {
    public static void main(String[] args) {

        Sender router = Sender.getInstance();
        Map<String, String> env = System.getenv();

        String identity,address;

        if (args.length > 0) {
            identity = args[0];
        } else {
            identity = env.getOrDefault("ID", "defaultID");
        }

        address = env.getOrDefault("IP", "172.25.0.2"); // todo: Not hardcode IP

        router.setIdentity(identity);
        router.setAddress(address);
        router.setPort(4321);

        router.setup();

        Root root = new Root();
        root.execute("");
    }
}
