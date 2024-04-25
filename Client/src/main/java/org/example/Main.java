package org.example;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Controller c = new Controller();
        c.start();

        Scanner sc = new Scanner(System.in);
        try (ZContext context = new ZContext();
             ZMQ.Socket sendCommand = context.createSocket(SocketType.PUB)) {

            sendCommand.bind("tcp://localhost:5001");
            while (sc.hasNextLine()) {
                String line = sc.nextLine();
                System.out.println("Send line to controller: " + line);
                sendCommand.send(line.getBytes());
            }
        }
    }
}