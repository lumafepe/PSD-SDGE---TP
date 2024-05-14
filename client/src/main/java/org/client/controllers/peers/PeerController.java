package org.client.controllers.peers;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.client.crdts.records.Rating;
import org.client.network.Broadcaster;
import org.client.messages.ClientMessage;
import org.client.crdts.Album;
import org.client.crdts.base.Operation;
import org.client.utils.VectorClock;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

public class PeerController {

    private static final Logger logger = LogManager.getLogger();

    private static final List<String> operations = Arrays.asList(
            "/addFile", "/removeFile", "/addUser", "/removeUser", "/chat", "/showAlbum");

    private final Broadcaster broadcaster;
    private final Album crdts = Album.getInstance();

    public PeerController(Broadcaster broadcaster) {
        // todo: add network for self operations
        this.broadcaster = broadcaster;
    }

    public boolean handles(String data) {
        for (String op: PeerController.operations) {
            if (data.startsWith(op)) {
                logger.info(String.format("handling peer operation '%s'", data));
                return true;
            }
        }
        return false;
    }

    public void handle(String data) {

        if (data.startsWith("/addFile")) {
            String rest = data.substring("/addFile".length());
            String[] restSplit = rest.split(" ");
            String fileName = restSplit[1];
            String content = restSplit[2];

            Operation o = new Operation("addFile", fileName);
            Operation newOp = crdts.handleOperation(o);

            try {
                broadcaster.broadcast(newOp);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        if (data.startsWith("/removeFile")) {
            String rest = data.substring("/removeFile".length());
            String[] restSplit = rest.split(" ");
            String fileName = restSplit[1];

            Operation o = new Operation("removeFile", fileName);
            Operation newOp = crdts.handleOperation(o);

            try {
                broadcaster.broadcast(newOp);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        if (data.startsWith("/addUser")) {
            String userName = data.substring("/addUser ".length());
            try {
                Operation o = new Operation("addUser", userName);
                Operation newOp = crdts.handleOperation(o);
                broadcaster.broadcast(newOp);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        if (data.startsWith("/removeUser")) {
            String userName = data.substring("/removeUser ".length());
            try {
                Operation o = new Operation("removeUser", userName);
                Operation newOp = crdts.handleOperation(o);
                broadcaster.broadcast(newOp);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        if (data.startsWith("/showAlbum")){
            System.out.println(crdts.toString());
        }

        if (data.startsWith("/chat")) {
            String message = data.substring("/chat ".length());
            try {
                broadcaster.broadcast(new Operation("chat", message));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        if (data.startsWith("/rate")) {

            String[] split = data.substring("/rate".length()).split(" ");

            String username = split[1];
            String filename = split[2];

            if (!crdts.getVotersCRDT().canRate(username)) {
                return; // if user already voted, then ignore rate request for filename
            }

            crdts.getFileRatingsCRDT().increment(filename); // rate the file by incrementing the counter
            crdts.getVotersCRDT().addRating(filename, username, crdts.getFileRatingsCRDT().getValue(filename)); // set user as already voted in the GOSet

            Operation<Rating> op = new Operation<>("rate", new Rating(username, filename, crdts.getFileRatingsCRDT().getValue(filename)));
            try {
                broadcaster.broadcast(op);
            } catch (IOException e) { throw new RuntimeException(e); }
        }
    }

    public ClientMessage handleIncoming(byte[] data) {
        ClientMessage messageReceived;

        try {
            messageReceived = ClientMessage.fromBytes(data);
        } catch (IOException | ClassNotFoundException e) {
            throw new RuntimeException(e);
        }

        return messageReceived;
    }

    public void receiveMessage(ClientMessage messageReceived){
        if (!messageReceived.type().equals("join") && !messageReceived.type().equals("forward") && !messageReceived.type().equals("state") && !messageReceived.type().equals("informJoin") && !messageReceived.type().equals("leave")) {
            broadcaster.receive(messageReceived.message());
        }
    }
}
