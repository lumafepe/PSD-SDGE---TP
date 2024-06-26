package org.client.threads;

import org.client.controllers.peers.PeerManagementController;
import org.client.messages.ClientMessage;
import org.client.controllers.peers.PeerController;
import org.client.crdts.CRDTS;
import org.client.utils.VectorClock;
import org.messages.central.*;
import org.client.messages.IncomingMessage;
import org.client.crdts.Album;
import org.client.network.Broadcaster;
import org.client.network.Network;
import org.client.controllers.server.ServerController;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;

public class Router extends Thread {

    private final String bindPort;

    private final Network network;
    private final Album operations;
    private Broadcaster broadcaster = null; // todo: do not forget to init this after /edit

    private final ServerController server;
    private final PeerController peerController;
    private final PeerManagementController peerManagementController;

    private VectorClock joinTimestamp;
    private String newNodeIdentity;
    private VectorClock receivedTimestamps;

    private byte[] uiIdentity;

    public Router(String bindPort, String serverAddress, int serverPort) {
        this.bindPort = bindPort;

        this.network = new Network(bindPort, this.bindPort, new ArrayList<>());
        this.operations = Album.getInstance();
        this.operations.setNodeId(bindPort);
        this.broadcaster = new Broadcaster(this.network, 0, 0);

        this.server = new ServerController(serverAddress, serverPort, "localhost", Integer.parseInt(bindPort));
        this.peerController = new PeerController(this.broadcaster, this.server);
        this.peerManagementController = new PeerManagementController(this.network, this.operations, this.bindPort, this.broadcaster, this.server, this.peerController);
    }

    private void routeMessage(IncomingMessage message) throws IOException {
        String messageData = new String(message.data());
        //System.out.println("Received message: " + new String(message.identity(), StandardCharsets.UTF_8) + " -> " + messageData);
        if (messageData.equals("/hello")){
            this.network.send("helloACK".getBytes(), message.identity());
            return;
        }
        if (messageData.equals("helloACK"))
            return;
        if (this.server.handles(messageData)) {
            if (messageData.startsWith("/leaveAlbum") && this.network.totalUsers() != 0){
                // Check if it can leave
                /*if (!this.broadcaster.canLeave()){
                    System.out.println("Cannot leave!");
                    return;
                }*/

                //if (this.network.totalUsers() != 0){
                    //this.peerManagementController.setIsLeaving(true);
                    ClientMessage leaveMessage = new ClientMessage("leave", null, null, this.bindPort, -1, -1, this.broadcaster.getVersion(), null);
                    try {
                        this.network.loopSend(leaveMessage.asBytes());
                        this.peerManagementController.setIsLeaving(true);
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                    Message m = Message.newBuilder().setType(Type.SUCESIUM).build();
                    this.network.self(message.identity(), m);
                    return;
                //}
            }
            Message reply = this.server.handle(messageData);

            if (reply != null) {
                if (this.peerManagementController.handlesServerReply(reply)) {
                    this.peerManagementController.handleEditReply(reply, this.broadcaster);
                }

                this.network.self(message.identity(), reply);
                // todo: else log that an unknown operation has been sent
            }
        }
        else if (this.peerController.handles(messageData) && this.server.getCurrentAlbum() != null) {
            Message m = this.peerController.handle(messageData);
            this.network.self(message.identity(), m);
        }
        else if (this.server.getCurrentAlbum() != null){
            ClientMessage incMessage = this.peerController.handleIncoming(message.data());
            this.peerController.receiveMessage(incMessage);

            if (this.peerManagementController.handlesPeerMessage(incMessage)){
                String msgType = this.peerManagementController.handlePeerMessage(incMessage, this.server.clock, this.server.position);
                Message m = Message.newBuilder().setType(Type.SUCESIUM).build();
                this.network.self(this.uiIdentity, m);
            }
        }

    }

    private boolean forwardMessage(ClientMessage message){
        // Only forward if has node to forward
        if (this.newNodeIdentity != null){
            VectorClock messageVC = message.message().version();
        }
        return false;
    }

    public void run() {
        try {
            while (true) {
                IncomingMessage message = this.network.recv();
                this.routeMessage(message);
            }
        } catch (Exception e){
            e.printStackTrace();
        }
    }
}
