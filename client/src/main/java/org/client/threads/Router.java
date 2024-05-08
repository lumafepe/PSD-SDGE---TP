package org.client.threads;

import org.client.controllers.PeerManagementController;
import org.client.messages.ClientMessage;
import org.client.controllers.PeerController;
import org.client.crdts.CRDTS;
import org.messages.central.*;
import org.client.utils.IncomingMessage;
import org.client.crdts.Album;
import org.client.network.Broadcaster;
import org.client.network.Network;
import org.client.controllers.ServerController;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Router extends Thread {

    private final String bindPort;

    private final Network network;
    private final Album operations;
    private Broadcaster broadcaster = null; // todo: do not forget to init this after /edit

    private final ServerController server;
    private final PeerController peerController;
    private final PeerManagementController peerManagementController;

    public Router(String bindPort, String serverAddress, int serverPort) {
        this.bindPort = bindPort;

        this.network = new Network(bindPort, this.bindPort, new ArrayList<>());
        this.operations = Album.getInstance();
        this.operations.setNodeId(bindPort);
        this.broadcaster = new Broadcaster(this.network, 0, 0);

        this.server = new ServerController(serverAddress, serverPort, "localhost", Integer.parseInt(bindPort));
        this.peerController = new PeerController(this.broadcaster);
        this.peerManagementController = new PeerManagementController(this.network, this.operations, this.bindPort);
    }

    private void routeMessage(IncomingMessage message) throws IOException {
        String messageData = new String(message.data());
        if (this.server.handles(messageData)) {
            Message reply = this.server.handle(messageData);

            if (reply != null){
                if (this.peerManagementController.handlesServerReply(reply)) {
                    this.peerManagementController.handleEditReply(reply, this.broadcaster);
                }

                this.network.self(message.identity(), reply);
                // todo: else log that an unknow operation has been sent
            }
        }
        else if (this.peerController.handles(messageData)){
            this.peerController.handle(messageData);
            Message m = Message.newBuilder().build();
            this.network.self(message.identity(), m);
        }
        else {
            ClientMessage incMessage = this.peerController.handleIncoming(message.data());

            if (this.peerManagementController.handlesPeerMessage(incMessage)){
                this.peerManagementController.handlePeerMessage(incMessage);
            }

            if (incMessage.type().equals("state")){
                CRDTS crdts = incMessage.crdts();
                this.operations.setCrdts(crdts);
            }
        }

    }

    public void run() {
        try {
            while (true) {
                IncomingMessage message = this.network.recv();
                System.out.println("received from client " + new String(message.identity()));
                this.routeMessage(message);
            }
        } catch (Exception e){
            IncomingMessage m = new IncomingMessage("".getBytes(), "/logout".getBytes());
        }
    }
}
