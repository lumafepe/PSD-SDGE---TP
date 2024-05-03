package org.example.Threads;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.io.IOException;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Controller c = null;
        try {
            c = new Controller(args[0]);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        c.start();

        Scanner sc = new Scanner(System.in);
        try (ZContext context = new ZContext();
             ZMQ.Socket sendCommand = context.createSocket(SocketType.REQ)) {
            sendCommand.connect("tcp://localhost:" + args[0]);
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