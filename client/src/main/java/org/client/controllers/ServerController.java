package org.client.controllers;

import org.messages.central.*;

import java.io.IOException;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.List;


public class ServerController {

    private static final List<String> operations = Arrays.asList(
            "/register", "/login", "/logout", "/listAlbums", "/createAlbum", "/getAlbum");

    private Socket serverSocket = null;

    public ServerController(String address, int port) {
        /*try {
            this.serverSocket = new Socket(address, port);
        }
        catch (IOException e) {
            e.printStackTrace(); // todo: replace with logger call
            System.exit(1);
        }*/
    }

    public boolean handles(String data) {
        for (String op: ServerController.operations) {
            if (data.startsWith(op)) {
                return true;
            }
        }
        return false;
    }

    public Message handle(String data) {

        if (data.startsWith("/register")) {
            return this.send(ServerOperations.register(data));
        }

        if (data.startsWith("/login")) {
            return this.send(ServerOperations.login(data));
        }

        if (data.startsWith("/logout")) {
            return this.send(ServerOperations.logout(data));
        }

        if (data.startsWith("/listAlbums")) {
            return this.send(ServerOperations.listAlbums(data));
        }

        if (data.startsWith("/createAlbum")) {
            return this.send(ServerOperations.createAlbum(data));
        }

        if (data.startsWith("/getAlbum")) {
            return this.send(ServerOperations.getAlbum(data));
        }

        // TODO: editMessage

        return null;
    }

    public Message send(Message message) {

        try {
            this.serverSocket.getOutputStream().write(message.toByteArray());

            byte[] buffer = new byte[16384]; // todo: dynamic buffer size ?
            int read = this.serverSocket.getInputStream().read(buffer);

            return Message.parseFrom(ByteBuffer.wrap(buffer, 0, read));
        }
        catch (IOException e) {
            e.printStackTrace(); // todo: replace with logger call
            System.exit(1);
        }

        return null;
    }

}
