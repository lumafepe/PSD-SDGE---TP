package org.client.threads;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.util.Scanner;

public class Main {

    public static void main(String[] args) {
        //new Router(args[0], "localhost", 4321).start();

        Scanner sc = new Scanner(System.in);
        try (ZContext context = new ZContext();
             ZMQ.Socket sendCommand = context.createSocket(SocketType.REQ)) {
            sendCommand.connect("tcp://localhost:6000");
            sendCommand.setIdentity("main".getBytes());
            while (sc.hasNextLine()) {
                String line = sc.nextLine();
                System.out.println("Send line to controller: " + line);
                sendCommand.sendMore("6000".getBytes());
                sendCommand.sendMore("");
                sendCommand.send(line, 0);

                byte[] reply = sendCommand.recv(0);
                System.out.println("Received reply from ROUTER: " + new String(reply));
            }
        }

    }
}