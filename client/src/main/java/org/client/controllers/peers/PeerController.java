package org.client.controllers.peers;

import org.client.network.Broadcaster;
import org.client.messages.ClientMessage;
import org.client.crdts.Album;
import org.client.crdts.base.Operation;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

public class PeerController {
    private static final List<String> operations = Arrays.asList(
            "/addFile", "/removeFile", "/addUser", "/removeUser", "/chat", "/showAlbum");
    private Broadcaster broadcaster;
    private final Album crdts = Album.getInstance();

    public PeerController(Broadcaster broadcaster) {
        // todo: add network for self operations
        this.broadcaster = broadcaster;
    }

    public boolean handles(String data) {
        for (String op: PeerController.operations) {
            if (data.startsWith(op)) {
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
    }

    public ClientMessage handleIncoming(byte[] data) {
        ClientMessage messageReceived;

        try {
            messageReceived = ClientMessage.fromBytes(data);
        } catch (IOException e) {
            throw new RuntimeException(e);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }

        if (!messageReceived.type().equals("join") && !messageReceived.type().equals("forward") && !messageReceived.type().equals("state")) {
            broadcaster.receive(messageReceived.message());
        }

        return messageReceived;
    }
}
