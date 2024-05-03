package org.example.Threads;

import client.messages.*;
import client.operations.*;
import com.google.protobuf.InvalidProtocolBufferException;
import org.example.CBcast;
import org.example.CRDTs.GOSet;
import org.example.CRDTs.ORset;
import org.example.CRDTs.Operation;
import org.example.CRDTs.VectorClock;
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
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.google.protobuf.ByteString;

public class Controller extends Thread{
    String bindPort;
    GOSet fileRatingsCRDT;
    ORset filesCRDT;
    ORset usersCRDT;
    CBcast causalBroadcast;
    Socket centralServer;
    PrintWriter out;
    BufferedReader in;
    List<String> sessionUsers;

    public Controller(String port) throws IOException {
        this.bindPort = port;
        this.fileRatingsCRDT = new GOSet();
        this.filesCRDT = new ORset();
        this.usersCRDT = new ORset();
        this.causalBroadcast = new CBcast();
        centralServer = new Socket("localhost", 4321);
        out = new PrintWriter(centralServer.getOutputStream(), true);
        in = new BufferedReader(new InputStreamReader(centralServer.getInputStream()));
        sessionUsers = new ArrayList<>();
        sessionUsers.add("6000");
        sessionUsers.add("6001");
        sessionUsers.remove(bindPort);
    }

    // Sockets Sub, Pub
    public void run() {
        System.out.println("Controller started working at port: " + bindPort);
        try (ZContext context = new ZContext();
             ZMQ.Socket router = context.createSocket(SocketType.ROUTER)) {
            router.bind("tcp://localhost:" + bindPort);
            router.setIdentity(bindPort.getBytes());

            for (String sessionUser : sessionUsers){
                router.connect("tcp://localhost:" + sessionUser);
            }

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
                    String rest = msgReceived.substring("/register".length());
                    String[] restSplit = rest.split(" ");
                    String username = restSplit[1];
                    System.out.println("REGISTER\nUsername: " + username);
                    String password = restSplit[2];
                    System.out.println("Password: " + password);

                    Message reply = send(Message.newBuilder()
                            .setType(Type.REGISTER)
                            .setRegister(Register.newBuilder()
                                    .setUsername(username)
                                    .setPassword(password)
                                    .build())
                            .build());

                    router.sendMore(identity);
                    router.sendMore("");
                    router.send(reply.toString(), 0);

                }
                else if (msgReceived.startsWith("/login")){
                    String rest = msgReceived.substring("/login".length());
                    String[] restSplit = rest.split(" ");
                    String username = restSplit[1];
                    System.out.println("LOGIN\nUsername: " + username);
                    String password = restSplit[2];
                    System.out.println("Password: " + password);

                    Message reply = send(Message.newBuilder()
                            .setType(Type.LOGIN)
                            .setLogin(Login.newBuilder()
                                    .setUsername(username)
                                    .setPassword(password)
                                    .build())
                            .build());

                    router.sendMore(identity);
                    router.sendMore("");
                    router.send(reply.toString(), 0);

                }
                else if (msgReceived.startsWith("/logout")){
                    Message reply = send(Message.newBuilder()
                            .setType(Type.LOGOUT)
                            .build());

                    router.sendMore(identity);
                    router.sendMore("");
                    router.send(reply.toString(), 0);

                }
                else if (msgReceived.startsWith("/listAlbums")){

                    Message reply = send(Message.newBuilder()
                            .setType(Type.ALBUMSLIST)
                            .build());

                    router.sendMore(identity);
                    router.sendMore("");
                    router.send(reply.toString(), 0);

                }
                else if (msgReceived.startsWith("/createAlbum")){
                    String rest = msgReceived.substring("/register".length());
                    String[] restSplit = rest.split(" ");
                    String albumName = restSplit[1];

                    Message reply = send(Message.newBuilder()
                            .setType(Type.ALBUMCREATE)
                            .setAlbumCreate(AlbumCreate.newBuilder()
                                    .setName(albumName)
                                    .build())
                            .build());

                    router.sendMore(identity);
                    router.sendMore("");
                    router.send(reply.toString(), 0);

                }
                else if (msgReceived.startsWith("/getAlbum")){
                    String rest = msgReceived.substring("/register".length());
                    String[] restSplit = rest.split(" ");
                    String albumName = restSplit[1];

                    Message reply = send(Message.newBuilder()
                            .setType(Type.ALBUMGET)
                            .setAlbumGet(AlbumGet.newBuilder()
                                    .setName(albumName)
                                    .build())
                            .build());

                    router.sendMore(identity);
                    router.sendMore("");
                    router.send(reply.toString(), 0);

                }
                else if (msgReceived.startsWith("/addFile")) {
                    String rest = msgReceived.substring("/addFile".length());
                    String[] restSplit = rest.split(" ");
                    String fileName = restSplit[1];
                    System.out.println("ADD FILE\nFilename: " + fileName);
                    String content = restSplit[2];
                    System.out.println("Content: " + content);

                    Operation o = filesCRDT.addElement("addFile", fileName, bindPort);
                    Iterable<VectorClock> observed = (List<VectorClock>) o.observed;
                    Client.OperationMessage.Builder builder = Client.OperationMessage.newBuilder()
                            .setElement(o.element)
                            .setOperation(o.operation)
                            .setVectorClock(Client.VectorClock.newBuilder()
                                    .setCounter(o.vectorClock.counter)
                                    .setNodeId(o.vectorClock.nodeId)
                                    .build());

                    if (observed != null) {
                        for (VectorClock vectorClock : observed) {
                            builder = builder.addObserved(Client.VectorClock.newBuilder()
                                    .setNodeId(vectorClock.nodeId)
                                    .setCounter(vectorClock.counter)
                                    .build());
                        }
                    }


                    Client.OperationMessage om = builder.build();
                    byte[] msg = om.toByteArray();

                    for (String userIdentity : sessionUsers){
                        router.sendMore(userIdentity);
                        router.sendMore("");
                        router.send(msg, 0);
                    }
                }
                else if (msgReceived.startsWith("/removeFile")) {
                    String fileName = msgReceived.substring("/removeFile".length());
                    System.out.println("File: " + fileName);
                }
                else if (msgReceived.startsWith("/getFiles")) {
                    System.out.println("Files: " + filesCRDT.elements());
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

                    //byte[] operationSerialized = o.serialize();
                    //System.out.println("Serialized data: " + operationSerialized);
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
                    try {
                        Client.OperationMessage operationReceived = Client.OperationMessage.parseFrom(request);
                        System.out.println(operationReceived);

                        if (operationReceived.getOperation().equals("addFile")){
                            List<Client.VectorClock> observedList = operationReceived.getObservedList();
                            Set<VectorClock> observedVectors = new HashSet<>();
                            for (Client.VectorClock vectorClock : observedList) {
                                observedVectors.add(new VectorClock(vectorClock.getNodeId(), (int)vectorClock.getCounter()));
                            }
                            Operation o = new Operation(operationReceived.getOperation(),
                                                        operationReceived.getElement(),
                                    new VectorClock(operationReceived.getVectorClock().getNodeId(),
                                            operationReceived.getVectorClock().getCounter()),
                                    observedVectors
                                    );

                            filesCRDT.applyAddOperation(o);
                        }
                    } catch (InvalidProtocolBufferException e) {
                        throw new RuntimeException(e);
                    }
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
