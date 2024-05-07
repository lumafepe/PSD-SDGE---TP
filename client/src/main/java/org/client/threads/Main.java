package org.client.threads;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        new Router(args[0], "127.0.0.1", 6002).start();

        Scanner sc = new Scanner(System.in);
        try (ZContext context = new ZContext();
             ZMQ.Socket sendCommand = context.createSocket(SocketType.REQ)) {
            sendCommand.connect("tcp://localhost:" + args[0]);
            sendCommand.setIdentity("main".getBytes());
            while (sc.hasNextLine()) {
                String line = sc.nextLine();
                System.out.println("Send line to controller: " + line);
                sendCommand.send(line.getBytes());

                byte[] reply = sendCommand.recv(0);
                System.out.println("Received reply from ROUTER: " + new String(reply));
            }
        }
    }
}