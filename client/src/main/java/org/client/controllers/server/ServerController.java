package org.client.controllers.server;

import org.client.crdts.Album;
import org.messages.central.*;

import java.io.IOException;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.List;


public class ServerController {

    private static final List<String> operations = Arrays.asList(
            "/register", "/login", "/logout", "/listAlbums", "/createAlbum", "/getAlbum", "/editAlbum", "/leaveAlbum");

    private Socket serverSocket = null;
    public int clock = 0;
    public int position = 0;
    private String clientIp;
    private int clientPort;
    private String currentAlbum;

    public ServerController(String address, int port, String clientIp, int clientPort) {
        this.clientIp = clientIp;
        this.clientPort = clientPort;

        try {
            this.serverSocket = new Socket(address, port);
        }
        catch (IOException e) {
            e.printStackTrace(); // todo: replace with logger call
            System.exit(1);
        }
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
            return this.send(ServerOperations.login(data, this.clientIp, this.clientPort));
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
            //return this.send(ServerOperations.getAlbum(data));
        }

        if (data.startsWith("/editAlbum")) {
            Message m = this.send(ServerOperations.editAlbum(data));
            if (m.getType() == Type.NEWCLIENT){
                this.clock = m.getNewClient().getClock();
                this.position = m.getNewClient().getPosition();
            }
            this.currentAlbum = data.substring("/editAlbum ".length());
            return m;
        }

        if (data.startsWith("/leaveAlbum")){
            this.currentAlbum = null;
            return this.send(ServerOperations.leaveAlbum(data, this.clock, this.position, Album.getInstance()));
        }

        return null;
    }

    public String getCurrentAlbum(){
        return this.currentAlbum;
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
