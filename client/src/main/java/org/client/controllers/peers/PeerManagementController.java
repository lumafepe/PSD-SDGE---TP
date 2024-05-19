package org.client.controllers.peers;

import org.client.controllers.server.ServerController;
import org.client.crdts.*;
import org.client.crdts.records.FileRecord;
import org.client.network.Broadcaster;
import org.client.network.Network;
import org.client.messages.ClientMessage;
import org.messages.central.*;

import java.io.IOException;
import java.util.*;

public class PeerManagementController {
    private static final List<String> peerMessages = Arrays.asList(
            "join", "informJoin", "state", "leave", "forward", "leaveAck", "leaveConfirm");

    private Network network;
    private Album album;
    private String identity;
    private Broadcaster broadcaster;
    private ServerController server;
    private String currentAlbum;
    private Set<String> acksReceived = new HashSet<>();
    private boolean isLeaving = false;
    private PeerController peerController;

    public PeerManagementController(Network network, Album album, String identity, Broadcaster broadcaster, ServerController server, PeerController peerController) {
        this.network = network;
        this.album = album;
        this.identity = identity;
        this.broadcaster = broadcaster;
        this.server = server;
        this.peerController = peerController;
    }

    public boolean handlesPeerMessage(ClientMessage message) {
        for (String op: PeerManagementController.peerMessages) {
            if (message.type().equals(op)) {
                return true;
            }
        }
        return false;
    }

    public String handlePeerMessage(ClientMessage message, int myClock, int myPosition) throws IOException {
        switch (message.type()) {
            case "join" -> {
                String newPeerIdentity = message.identity();
                int position = message.position();
                int clockValue = message.clock();

                // Message informing other nodes that a new node has joined
                ClientMessage informMessage = new ClientMessage("informJoin", null, null, newPeerIdentity, clockValue, position, null, null);
                this.broadcaster.addToVector(position, clockValue);
                this.broadcaster.addForwardingNode(message.identity());
                this.network.loopSend(informMessage.asBytes());

                this.network.addUser(newPeerIdentity);

                // Send state to joining node
                ClientMessage clientMessage = new ClientMessage("state", null, album.getCrdts(), identity, -1, -1, this.broadcaster.getVersion(), this.broadcaster.getPending());
                // todo: must send also pending and keep forwarding messages
                this.network.send(clientMessage.asBytes(), newPeerIdentity);

            }
            case "informJoin" -> {
                String newPeerIdentity = message.identity();
                this.network.addUser(newPeerIdentity);
                this.broadcaster.addToVector(message.position(), message.clock());
            }
            case "state" -> {
                CRDTS crdts = message.crdts();
                this.album.setCrdts(crdts);
                this.broadcaster.setVersion(message.vc(), myClock, myPosition);
                this.broadcaster.setPending(message.pending());

                // todo: must send also pending and keep forwarding messages
            }
            case "leave" -> {
                //this.network.removeUser(message.identity());
                if (this.isLeaving && Integer.parseInt(message.identity()) > Integer.parseInt(this.identity)){
                    this.isLeaving = false;
                }
                //this.broadcaster.addWaitingMsg(message.vc());
                ClientMessage leaveAck = new ClientMessage("leaveAck", null, null, this.identity, -1, -1, null, null);
                this.network.send(leaveAck.asBytes(), message.identity());
            }
            case "leaveAck" -> {
                if (isLeaving){
                    this.acksReceived.add(message.identity());
                    if (this.acksReceived.size() == this.network.size()) {
                        this.server.handle("/leaveAlbum " + this.server.getCurrentAlbum());
                        ClientMessage leaveConfirm = new ClientMessage("leaveConfirm", null, null, this.identity, -1, -1, null, null);
                        this.network.loopSend(leaveConfirm.asBytes());
                    }
                }
            }
            case "leaveConfirm" -> {
                this.network.removeUser(message.identity());
            }
            case "forward" -> {
                this.broadcaster.receive(message.message());
            }

        }
        return message.type();
    }

    public boolean handlesServerReply(Message reply) {
        return reply.getType() == Type.ALBUM || reply.getType() == Type.NEWCLIENT;
    }

    public void handleEditReply(Message reply, Broadcaster broadcaster) throws IOException {
        // When he is the first in the session
        if (reply.getType() == Type.ALBUM){
            AlbumMessage album = reply.getAlbum();

            // Get users
            List<String> users = new ArrayList<>(album.getUsersList());
            this.album.setUsers(users, this.identity);

            // Get files
            GCounter gc = new GCounter();
            Map<String, GOSet> voters = new HashMap<>();
            List<FileRecord> files = new ArrayList<>();

            for (File f : album.getFilesList()) {
                files.add(new FileRecord(f.getName(), f.getHash()));
                voters.put(f.getName(), new GOSet());

                // counters for classifications
                for (Classification c : f.getClassificationsList()){
                    gc.increment(f.getName(), c.getValue()); // Increment the rating on the crdt
                    voters.get(f.getName()).addRating(f.getName(), c.getUsername(), c.getValue()); // set the rate operation onto the crdt
                }
            }

            this.album.setFiles(files, this.identity);
            this.album.setFileRatingsCRDT(gc);
            this.album.setVotersCRDT(voters);
        }

        // When he is not the first in the session
        if (reply.getType() == Type.NEWCLIENT){
            NewClient newClientPart = reply.getNewClient();

            int vectorPosition = newClientPart.getPosition();
            int vectorInitialValue = newClientPart.getClock();
            List<Client> clients = newClientPart.getClientsList();
            Client mediator = clients.get(0);

            for (Client client : clients){
                this.network.addUser(String.valueOf(client.getPort()));
            }

            ClientMessage joinMessage = new ClientMessage("join", null, null, this.identity, vectorInitialValue, vectorPosition, null, null);

            try {
                this.network.send(joinMessage.asBytes(), String.valueOf(mediator.getPort()));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

            broadcaster.initialize(vectorPosition, vectorInitialValue);
        }
    }

    public void setIsLeaving(boolean b){
        this.isLeaving = b;
    }
}
