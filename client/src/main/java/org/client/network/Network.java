package org.client.network;

import org.messages.central.*;
import org.client.messages.IncomingMessage;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.util.ArrayList;
import java.util.List;

public class Network {

    private static final String BASE_ADDRESS = "tcp://localhost:";

    private final ZContext ctx = new ZContext();
    private ZMQ.Socket router;

    private List<String> users = new ArrayList<>();
    private String myIdentity;

    private ZMQ.Poller poller;

    public Network(String identity, String bindPort, List<String> users) {
        this.myIdentity = identity; // Just For debugging
        this.router = ctx.createSocket(SocketType.ROUTER);
        this.router.bind(Network.BASE_ADDRESS + bindPort);
        this.router.setIdentity(this.myIdentity.getBytes());

        poller = ctx.createPoller(1);
        poller.register(router, ZMQ.Poller.POLLIN);

        this.users = users;

        for (String user : this.users){
            connect(user);
        }
    }

    public int totalUsers(){
        return users.size();
    }

    public void self(byte[] identity, Message message) {
        router.sendMore(identity);
        router.sendMore("");
        router.send(message.toByteArray(), 0);
    }

    public void loopSend(byte[] data) {
        for (String identity : this.users) {
            router.sendMore(identity.getBytes());
            router.sendMore("");
            router.send(data, 0);
        }
    }

    public void send(byte[] data, String identity) {
        router.sendMore(identity.getBytes());
        router.sendMore("");
        router.send(data, 0);
    }

    public void send(byte[] data, byte[] identity){
        router.sendMore(identity);
        router.sendMore("");
        router.send(data, 0);
    }

    public void addUser(String port) {
        if (!this.users.contains(port)) {
            this.users.add(port);
            connect(port);
        }
    }

    public void removeUser(String user) {
        boolean removed = this.users.remove(user);
        if (removed) {
            this.router.disconnect(Network.BASE_ADDRESS + user);
        }
    }

    public void connect(String other){
        this.router.connect(Network.BASE_ADDRESS + other);
        boolean connected = false;
        while (!connected){
            router.sendMore(other.getBytes());
            router.sendMore("");
            router.send("/hello");
            int events = poller.poll(100); // Wait for 1 second
            if (events > 0) {
                if (poller.pollin(0)) {
                    // Receive the acknowledgment
                    String address = router.recvStr();
                    router.recvStr(); // Empty delimiter
                    String response = router.recvStr();

                    if (response != null) {
                        connected = true;
                        System.out.println("Connected to " + other);
                    }
                }
            }
        }

    }

    public int size() {
        return users.size();
    }

    public IncomingMessage recv() {
        byte[] identity = this.router.recv(0);
        this.router.recv(0); // discard message delimiter
        byte[] data = this.router.recv(0);

        return new IncomingMessage(identity, data);
    }
}
