package org.client.threads;

import org.client.CBcast;
import org.client.crdts.GOSet;
import org.client.crdts.ORset;
import org.client.crdts.Operation;
import org.client.Rating;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class Controller extends Thread{
    GOSet fileRatingsCRDT;
    ORset filesCRDT;
    ORset usersCRDT;
    CBcast causalBroadcast;

    public Controller() {
        this.fileRatingsCRDT = new GOSet();
        this.filesCRDT = new ORset();
        this.usersCRDT = new ORset();
        this.causalBroadcast = new CBcast();
    }

    // Sockets Sub, Pub
    public void run() {
        System.out.println("Controller started working");
        try (ZContext context = new ZContext();
             ZMQ.Socket subscriber = context.createSocket(SocketType.SUB);
             ZMQ.Socket broadcast = context.createSocket(SocketType.PUSH)) {
            subscriber.connect("tcp://localhost:5001");
            subscriber.connect("tcp://localhost:5003");
            subscriber.subscribe("");
            broadcast.connect("tcp://localhost:5002");

            while (true) {
                byte[] line = subscriber.recv();
                String msgReceived = new String(line);

                if (msgReceived.startsWith("/chat")) {
                    continue;
                }
                else if (msgReceived.startsWith("/addFile")) {
                    String fileName = msgReceived.substring("/addFile".length());
                    System.out.println("File: " + fileName);
                }
                else if (msgReceived.startsWith("/removeFile")) {
                    String fileName = msgReceived.substring("/removeFile".length());
                    System.out.println("File: " + fileName);
                }
                else if (msgReceived.startsWith("/addUser")) {
                    String userName = msgReceived.substring("/addUser".length());
                    System.out.println("User: " + userName);
                }
                else if (msgReceived.startsWith("/removeUser")) {
                    String userName = msgReceived.substring("/removeUser".length());
                    System.out.println("User: " + userName);
                }
                else if (msgReceived.startsWith("/rate")) {
                    String rest = msgReceived.substring("/rate".length());
                    String[] restSplit = rest.split(" ");
                    String fileName = restSplit[1];
                    System.out.println("File: " + fileName);
                    String rating = restSplit[2];
                    System.out.println("Rating: " + rating);

                    String pid = String.valueOf(ProcessHandle.current().pid());
                    int intRating = Integer.parseInt(rating);
                    Operation o = this.fileRatingsCRDT.addRating(new Rating(pid, fileName, intRating));

                    byte[] operationSerialized = o.serialize();
                    System.out.println("Serialized data: " + operationSerialized);
                    broadcast.send(operationSerialized);
                }
                else if (msgReceived.startsWith("/listRates")) {
                    if (this.fileRatingsCRDT.getRatings().isEmpty()) {
                        System.out.println("No ratings found");
                    }
                    for (Rating r : this.fileRatingsCRDT.getRatings()) {
                        System.out.println(r);
                    }
                }
                else {
                    Operation o = Operation.deserialize(msgReceived.getBytes());
                    System.out.println("Received operation: " + o);
                }
            }
        }
    }
}
