package org.client.controllers;

import org.client.BroadcastMessage;
import org.client.Broadcaster;
import org.client.crdts.base.Operation;
import org.messages.central.Message;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

public class PeerController {
    private static final List<String> operations = Arrays.asList(
            "/addFile", "/removeFile", "/addUser", "/removeUser", "/chat");
    private Broadcaster broadcaster;

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

    public Message handle(String data) {

        if (data.startsWith("/addFile")) {

        }

        if (data.startsWith("/addFile")) {

        }

        if (data.startsWith("/addUser")) {

        }

        if (data.startsWith("/removeUser")) {

        }

        if (data.startsWith("/chat")) {
            String message = data.substring("/chat ".length());
            try {
                broadcaster.broadcast(new Operation("chat", message));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        return null;
    }

    public void handleIncoming(byte[] data) {
        BroadcastMessage messageReceived;

        try {
            messageReceived = BroadcastMessage.fromBytes(data);
        } catch (IOException e) {
            throw new RuntimeException(e);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }

        broadcaster.receive(messageReceived);
    }
}
