package org.client.threads;

import org.client.controllers.PeerController;
import org.messages.central.*;
import org.client.utils.IncomingMessage;
import org.client.crdts.Album;
import org.client.Broadcaster;
import org.client.Network;
import org.client.controllers.ServerController;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Router extends Thread {

    private final String bindPort;

    private final Network network;
    private final Album operations;
    private Broadcaster broadcaster = null; // todo: do not forget to init this after /edit

    private final ServerController server;
    private final PeerController peerController;

    public Router(String bindPort, String serverAddress, int serverPort) {
        this.bindPort = bindPort;

        this.network = new Network(this.bindPort, new ArrayList<>());
        this.network.addUser("6000");
        this.network.addUser("6001");
        this.network.removeUser(bindPort);
        this.operations = Album.getInstance();
        this.operations.setNodeId(bindPort);
        this.broadcaster = new Broadcaster(this.network, 0, 0);

        this.server = new ServerController(serverAddress, serverPort);
        this.peerController = new PeerController(this.broadcaster);
    }

    private void routeMessage(IncomingMessage message) {
        String messageData = new String(message.data());
        if (this.server.handles(messageData)) {
            Message reply = this.server.handle(messageData);

            if (reply != null){
                if (reply.getType() == Type.ALBUM){
                    AlbumMessage album = reply.getAlbum();

                    // Get users
                    List<String> users = new ArrayList<>();
                    for (String u : album.getUsersList()){
                        users.add(u);
                    }
                    operations.setUsers(users, bindPort);

                    // Get files
                    List<File> files = new ArrayList<>();
                    for (File f : album.getFilesList()){
                        files.add(f);
                    }
                    operations.setFiles(files, bindPort);
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
            this.peerController.handleIncoming(message.data());
        }

    }

    public void run() {

        while (true) {
            IncomingMessage message = this.network.recv();
            System.out.println("received from client " + new String(message.identity()));
            this.routeMessage(message);
        }
    }
}