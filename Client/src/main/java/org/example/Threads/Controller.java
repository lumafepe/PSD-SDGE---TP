package org.example.Threads;

import client.messages.Register;
import client.messages.Message;
import client.messages.Type;
import org.example.CBcast;
import org.example.CRDTs.GOSet;
import org.example.CRDTs.ORset;
import org.example.CRDTs.Operation;
import org.example.Rating;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;

import com.google.protobuf.ByteString;

public class Controller extends Thread{
    GOSet fileRatingsCRDT;
    ORset filesCRDT;
    ORset usersCRDT;
    CBcast causalBroadcast;
    Socket centralServer;
    PrintWriter out;
    BufferedReader in;

    public Controller() throws IOException {
        this.fileRatingsCRDT = new GOSet();
        this.filesCRDT = new ORset();
        this.usersCRDT = new ORset();
        this.causalBroadcast = new CBcast();
        centralServer = new Socket("localhost", 4321);
        out = new PrintWriter(centralServer.getOutputStream(), true);
        in = new BufferedReader(new InputStreamReader(centralServer.getInputStream()));
    }

    // Sockets Sub, Pub
    public void run() {
        System.out.println("Controller started working");
        try (ZContext context = new ZContext();
             ZMQ.Socket router = context.createSocket(SocketType.ROUTER)) {
            router.bind("tcp://localhost:5001");

            while (true) {
                byte[] identity = router.recv(0);

                // Discard empty delimiter frame
                router.recv(0);

                // Receive request message
                byte[] request = router.recv(0);
                String msgReceived = new String(request);
                System.out.println("Received request from client " + new String(identity) + ": " + new String(request));

                if (msgReceived.startsWith("/chat")) {
                    continue;
                }
                else if (msgReceived.startsWith("/register")){
                    Register registerRequest = Register.newBuilder()
                            .setUsername("miguel")
                            .setPassword("miguel")
                            .build();

                    Message reply = send(Message.newBuilder()
                            .setType(Type.REGISTER)
                            .setRegister(Register.newBuilder()
                                    .setUsername("miguel")
                                    .setPassword("miguel")
                                    .build())
                            .build());

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
                    //broadcast.send(operationSerialized);
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

    private Message send(Message msg) {
        try {
            centralServer.getOutputStream().write(msg.toByteArray());
            byte[] buffer = new byte[1048576];
            int read = centralServer.getInputStream().read(buffer);
            Message response = Message.parseFrom(ByteBuffer.wrap(buffer, 0, read));
            return response;
        } catch(UnknownHostException e) {
            System.exit(1);
        } catch(IOException e) {
            System.exit(1);
        }
        return null;
    }
}
