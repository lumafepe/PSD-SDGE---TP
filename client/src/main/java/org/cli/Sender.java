package org.cli;

import org.client.threads.Router;
import org.messages.central.Message;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.io.IOException;

public class Sender {

    private static Sender instance = null;

    private String address;
    private String identity;
    private int port;

    private Router router;
    private ZMQ.Socket requester;

    private Sender() {
        instance = this;
    }

    public static Sender getInstance() {
        if (instance == null) {
            instance = new Sender();
        }
        return instance;
    }

    public void setup() {
        ZContext ctx = new ZContext();
        this.requester = ctx.createSocket(SocketType.REQ);

        this.router = new Router(identity, address, port);
        this.router.start();
    }

    public Message send(String message) {
        this.requester.send(message.getBytes());

        try {
            return Message.parseFrom(this.requester.recv(0));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public void setPort(int port) {
        this.port = port;
    }

    public void setIdentity(String identity) {
        this.identity = identity;
    }

    public void set(String address, int port, String identity) {
        this.address = address;
        this.port = port;
        this.identity = identity;
    }
}