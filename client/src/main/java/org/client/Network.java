package org.client;

import client.central.Message;
import org.client.utils.IncomingMessage;
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

    public Network(String bindPort, List<String> users) {

        this.router.bind(Network.BASE_ADDRESS + bindPort);
        this.router.setIdentity(bindPort.getBytes());

        this.users = users;

        for (String user : this.users){
            router.connect(Network.BASE_ADDRESS + user);
        }
    }

    public void self(byte[] identity, Message message) {
        router.sendMore(identity);
        router.sendMore("");
        router.send(message.toString(), 0);
    }

    public void loopSend(byte[] data) {
        for (String identity : this.users) {
            router.sendMore(identity);
            router.sendMore("");
            router.send(data, 0);
        }
    }

    public void addUser(String user) {
        if (!this.users.contains(user)) {
            this.users.add(user);
            router.connect(Network.BASE_ADDRESS + user);
        }
    }

    public void removeUser(String user) {
        boolean removed = this.users.remove(user);
        if (removed) {
            this.router.disconnect(user);
        }
    }

    public int size() {
        return users.size();
    }

    public IncomingMessage recv() {

        byte[] identity = this.router.recv(0);
        this.router.recv(0); // discard message delimiter
        byte[] data = this.router.recv(0);

        return new IncomingMessage(identity, new String(data));
    }
}
