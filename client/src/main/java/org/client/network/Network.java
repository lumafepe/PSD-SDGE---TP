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
    private final ZMQ.Socket router = ctx.createSocket(SocketType.ROUTER);

    private List<String> users = new ArrayList<>();
    private String myIdentity;

    public Network(String identity, String bindPort, List<String> users) {
        this.myIdentity = identity; // Just For debugging
        this.router.bind(Network.BASE_ADDRESS + bindPort);
        this.router.setIdentity(identity.getBytes());

        this.users = users;

        for (String user : this.users){
            router.connect(Network.BASE_ADDRESS + user);
        }
    }

    public int totalUsers(){
        return users.size();
    }

    public void self(byte[] identity, Message message) {
        router.sendMore(identity);
        router.sendMore("");
        router.send(message.toString(), 0);
    }

    public void loopSend(byte[] data) {
        for (String identity : this.users) {
            /*if (this.myIdentity.equals("6000") && identity.equals("6002")) {
                try {
                    Thread.sleep(10000);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }*/
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

    public void addUser(String port) {
        if (!this.users.contains(port)) {
            this.users.add(port);
            router.connect(Network.BASE_ADDRESS + port);
        }
    }

    public void removeUser(String user) {
        boolean removed = this.users.remove(user);
        if (removed) {
            this.router.disconnect(Network.BASE_ADDRESS + user);
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
