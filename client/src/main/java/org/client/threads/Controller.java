/*package org.client.threads;

import org.messages.central.*;
import org.messages.p2p.*;

import org.client.messages.BroadcastMessage;
import org.client.network.Broadcaster;
import org.client.crdts.Album;
import org.client.crdts.base.Operation;
import org.client.crdts.GOSet;
import org.client.crdts.ORset;
import org.client.crdts.Rating;
import org.client.controllers.DHTController;
import org.client.controllers.ServerController;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;

public class Controller extends Thread {
    private final String bindPort;

    private Broadcaster broadcaster;
    private Album operations;

    private DHTController dht;
    private ServerController server;



    GOSet fileRatingsCRDT;
    ORset filesCRDT;
    ORset usersCRDT;
    Broadcaster causalBroadcast;
    Socket centralServer;
    List<String> sessionUsers;
    ZMQ.Socket router;

    public Controller(String port) throws IOException {
        this.bindPort = port;
        this.fileRatingsCRDT = new GOSet();
        this.filesCRDT = new ORset();
        this.usersCRDT = new ORset();
        ZContext context = new ZContext();
        this.router = context.createSocket(SocketType.ROUTER);
        router.bind("tcp://localhost:" + bindPort);
        router.setIdentity(bindPort.getBytes());

        this.sessionUsers = new ArrayList<>();
        this.sessionUsers.add("6000");
        this.sessionUsers.add("6001");
        this.sessionUsers.remove(bindPort);

        this.causalBroadcast = new Broadcaster(sessionUsers, Integer.parseInt(bindPort) % 6000, 0, this.router);
        centralServer = new Socket("localhost", 4321);

    }

    // Sockets Sub, Pub
    public void run() {
        System.out.println("Controller started working at port: " + bindPort);
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
                String message = msgReceived.substring("/chat ".length());
                try {
                    causalBroadcast.broadcast(new Operation("chat", message));
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
            else if (msgReceived.startsWith("/register")){
                String rest = msgReceived.substring("/register".length());
                String[] restSplit = rest.split(" ");
                String username = restSplit[1];
                String password = restSplit[2];

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
                String password = restSplit[2];

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
                String rest = msgReceived.substring("/createAlbum".length());
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
                String rest = msgReceived.substring("/getAlbum".length());
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
                String content = restSplit[2];

                Operation o = filesCRDT.addElement("addFile", fileName, bindPort);
                broadcast(router, identity, o);
            }
            else if (msgReceived.startsWith("/removeFile")) {
                String rest = msgReceived.substring("/removeFile".length());
                String[] restSplit = rest.split(" ");
                String fileName = restSplit[1];

                Operation o = filesCRDT.removeElement("removeFile", fileName);
                broadcast(router, identity, o);
            }
            else if (msgReceived.startsWith("/getFiles")) {
                router.sendMore(identity);
                router.sendMore("");
                router.send(filesCRDT.elements().toString(), 0);
            }
            else if (msgReceived.startsWith("/addUser")) {
                String rest = msgReceived.substring("/addUser".length());
                String[] restSplit = rest.split(" ");
                String userName = restSplit[1];

                Operation o = usersCRDT.addElement("addUser", userName, bindPort);
                broadcast(router, identity, o);
            }
            else if (msgReceived.startsWith("/removeUser")) {
                String rest = msgReceived.substring("/removeUser".length());
                String[] restSplit = rest.split(" ");
                String userName = restSplit[1];

                Operation o = usersCRDT.removeElement("removeUser", userName);
                broadcast(router, identity, o);
            }
            else if (msgReceived.startsWith("/getUsers")) {
                router.sendMore(identity);
                router.sendMore("");
                router.send(usersCRDT.elements().toString(), 0);
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
                Operation o = this.fileRatingsCRDT.addRating(fileName, bindPort, Integer.parseInt(rating));

                broadcast(router, identity, o);
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
                BroadcastMessage messageReceived = null;
                try {
                    messageReceived = BroadcastMessage.fromBytes(request);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                } catch (ClassNotFoundException e) {
                    throw new RuntimeException(e);
                }
                causalBroadcast.receive(messageReceived);
                //OperationMessage operationReceived = OperationMessage.parseFrom(request);
                //System.out.println(operationReceived);*/
                /*Operation o = messageReceived.operation();

                if (o.operation.equals("addFile")){
                    System.out.println("Received Operation from other client: " + o);
                    filesCRDT.applyAddOperation(o);
                }
                else if (o.operation.equals("removeFile")){
                    System.out.println("Received Operation from other client: " + o);
                    filesCRDT.applyAddOperation(o);
                }
                else if (o.operation.equals("addUser")){
                    System.out.println("Received Operation from other client: " + o);
                    usersCRDT.applyAddOperation(o);
                }
                else if (o.operation.equals("removeUser")){
                    System.out.println("Received Operation from other client: " + o);
                    usersCRDT.applyAddOperation(o);
                }
                else if (o.operation.equals("rate")){
                    System.out.println("Received Operation from other client: " + o);
                    fileRatingsCRDT.applyAddRatingOperation(o);
                }
                else if (o.operation.equals("chat")){
                    System.out.println("Received Operation from other client: " + o);
                }*//*
            }

        }
    }

    private void broadcast(ZMQ.Socket router, byte[] identity, Operation o) {
        OperationMessage om = o.getOperationMessage();

        byte[] msg = om.toByteArray();

        for (String userIdentity : sessionUsers){
            router.sendMore(userIdentity);
            router.sendMore("");
            router.send(msg, 0);
        }

        router.sendMore(identity);
        router.sendMore("");
        router.send("SUCCESS", 0);
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
}*/
