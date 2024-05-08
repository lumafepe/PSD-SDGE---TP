package org.client.controllers;

import org.client.crdts.Album;
import org.client.crdts.CRDTS;
import org.client.network.Broadcaster;
import org.client.network.Network;
import org.client.messages.ClientMessage;
import org.messages.central.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class PeerManagementController {
    private static final List<String> peerMessages = Arrays.asList(
            "join", "state", "leave", "forward");
    private Network network;
    private Album album;
    private String identity;

    public PeerManagementController(Network network, Album album, String identity) {
        this.network = network;
        this.album = album;
        this.identity = identity;
    }

    public boolean handlesPeerMessage(ClientMessage message) {
        for (String op: PeerManagementController.peerMessages) {
            if (message.type().equals(op)) {
                return true;
            }
        }
        return false;
    }

    public void handlePeerMessage(ClientMessage message) throws IOException {
        switch (message.type()) {
            case "join" -> {
                String newPeerIdentity = message.identity();
                this.network.addUser(newPeerIdentity);

                ClientMessage clientMessage = new ClientMessage("state", null, album.getCrdts(), identity);
                this.network.send(clientMessage.asBytes(), newPeerIdentity);
            }
            case "state" -> {
                CRDTS crdts = message.crdts();
                this.album.setCrdts(crdts);

                // todo: must send also pending and keep forwarding messages
            }
            case "leave" -> {
                // todo
                System.out.println("Leave");
            }
            case "forward" -> {
                // todo
                System.out.println("Forward");
            }
        }
    }

    public boolean handlesServerReply(Message reply) {
        return reply.getType() == Type.ALBUM || reply.getType() == Type.NEWCLIENT;
    }

    public void handleEditReply(Message reply, Broadcaster broadcaster) throws IOException {
        // When he is the first in the session
        if (reply.getType() == Type.ALBUM){
            AlbumMessage album = reply.getAlbum();

            // Get users
            List<String> users = new ArrayList<>();
            for (String u : album.getUsersList()){
                users.add(u);
            }
            this.album.setUsers(users, this.identity);

            // Get files
            List<File> files = new ArrayList<>();
            for (File f : album.getFilesList()){
                files.add(f);
            }
            this.album.setFiles(files, this.identity);
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

            ClientMessage bMessage = new ClientMessage("join", null, null, this.identity);
            try {
                this.network.send(bMessage.asBytes(), String.valueOf(mediator.getPort()));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

            broadcaster.initialize(vectorPosition, vectorInitialValue);
        }
    }
}
