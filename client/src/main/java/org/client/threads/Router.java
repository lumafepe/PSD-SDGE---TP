package org.client.threads;

import client.central.Login;
import client.central.Message;
import client.central.Type;
import org.client.utils.IncomingMessage;
import org.client.crdts.Album;
import org.client.Broadcaster;
import org.client.Network;
import org.client.controllers.ServerController;

import java.util.ArrayList;

public class Router extends Thread {

    private final String bindPort;

    private final Network network;
    private final Album operations;
    private final Broadcaster broadcaster = null; // todo: do not forget to init this after /edit

    private final ServerController server;

    public Router(String bindPort, String serverAddress, int serverPort) {
        this.bindPort = bindPort;

        this.network = new Network(this.bindPort, new ArrayList<>());
        this.operations = Album.getInstance();

        this.server = new ServerController(serverAddress, serverPort);
    }

    private void routeMessage(IncomingMessage message) {

        if (this.server.handles(message.data())) {
            Message reply = this.server.handle(message.data());

            if (reply != null)
                this.network.self(message.identity(), reply);
            // todo: else log that an unknow operation has been sent
        }

    }

    public void run() {

        while (true) {
            IncomingMessage message = this.network.recv();
            System.out.println("received from client " + new String(message.identity()) + ": " + message.data());
            this.routeMessage(message);
        }
    }

}
