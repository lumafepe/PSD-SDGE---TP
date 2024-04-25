package org.example;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class Controller extends Thread{
    GOSet fileRatingsCRDT;
    ORset filesCRDT;
    ORset usersCRDT;

    public Controller() {
        this.fileRatingsCRDT = new GOSet();
        this.filesCRDT = new ORset();
        this.usersCRDT = new ORset();
    }

    // Sockets Sub, Pub
    public void run() {
        System.out.println("Controller started working");
        try (ZContext context = new ZContext();
             ZMQ.Socket subscriber = context.createSocket(SocketType.SUB)) {
            subscriber.connect("tcp://localhost:5001");
            subscriber.subscribe("");

            while (true) {
                byte[] line = subscriber.recv();
                String msgReceived = new String(line);
                System.out.println("CONTROLLER: Received line: " + msgReceived);

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
                    this.fileRatingsCRDT.addRating(new Rating(pid, fileName, intRating));
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
                    System.out.println("Unknown message: " + msgReceived);
                }
            }
        }
    }
}
