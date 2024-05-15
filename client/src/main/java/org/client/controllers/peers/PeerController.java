package org.client.controllers.peers;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.client.controllers.DHTController;
import org.client.controllers.server.ServerController;
import org.client.crdts.records.File;
import org.client.crdts.records.Rating;
import org.client.network.Broadcaster;
import org.client.messages.ClientMessage;
import org.client.crdts.Album;
import org.client.crdts.base.Operation;
import org.client.utils.Hasher;
import org.client.utils.VectorClock;
import org.messages.central.NodeInfo;
import org.messages.central.NodeIp;
import org.messages.dht.Status;
import org.w3c.dom.Node;
import zmq.socket.Peer;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class PeerController {

    private static final Logger logger = LogManager.getLogger();

    private static final List<String> operations = Arrays.asList(
            "/addFile", "/getFile", "/removeFile", "/addUser", "/removeUser", "/chat", "/showAlbum", "/rate");

    private final Broadcaster broadcaster;
    private final ServerController server;
    private final PeerOperations handlers = new PeerOperations();

    private final Album crdts = Album.getInstance();

    public PeerController(Broadcaster broadcaster, ServerController server) {
        this.broadcaster = broadcaster;
        this.server = server;
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
            String[] split = data.substring("/addFile".length()).split(" ");
            handlers.addFile(split[1], split[2]);
                             //^filename //^filepath
        }

        if (data.startsWith("/getFile")) {
            String[] split = data.substring("/getFile".length()).split(" ");
            handlers.getFile(split[1], split[2]);
                             //^filepath //^destination
        }

        if (data.startsWith("/removeFile")) {
            String[] split = data.substring("/removeFile".length()).split(" ");
            handlers.removeFile(split[1]);
                                //^filename
        }

        if (data.startsWith("/addUser")) {
            String username = data.substring("/addUser ".length());
            handlers.addUser(username);
        }

        if (data.startsWith("/removeUser")) {
            String username = data.substring("/removeUser ".length());
            handlers.removeUser(username);
        }

            if (data.startsWith("/showAlbum")) {
                System.out.println(crdts.toString());
            }

        if (data.startsWith("/chat")) {
            String message = data.substring("/chat ".length());
            handlers.chat(message);
        }

        if (data.startsWith("/rate")) {
            String[] split = data.substring("/rate".length()).split(" ");
            handlers.rate(split[1], split[2], split[3]);
                          //^username //^filename //^value
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

    public void receiveMessage(ClientMessage messageReceived) {

        boolean receive = true;

        List<String> exclude = Arrays.asList("join", "forward", "state", "informJoin", "leave");
        for (String type : exclude) {
            receive = receive && !messageReceived.type().equals(type);
        }

        if (receive) {
            broadcaster.receive(messageReceived.message());
        }
    }

    private class PeerOperations {

        public void addFile(String filename, String filepath) {

            // get sha-1 hash for the file at filepath
            String hash = Hasher.digest(filepath);
            NodeInfo dht = server.getWriteAddressFor(hash); // get dht node ip:port

            // write file to dht
            Status result;
            try {
                result = DHTController.at(dht.getIp(), dht.getPort()).setFile(filepath);
            } catch (IOException e) { throw new RuntimeException(e); }

            // if failed to add the file on the dht, abort
            if (!result.equals(Status.SUCCESS)) {
                logger.error(String.format("add file '%s' ('%s') operation failed, dht status code: %s", filename, filepath, result));
                return;
            }

            Operation<String> o = new Operation<>("addFile", filename);
            Operation<File> crdtsOp = crdts.handleOperation(o);

            try { broadcaster.broadcast(crdtsOp); }
            catch (IOException e) { throw new RuntimeException(e); }

            logger.info(String.format("successfully added file '%s' ('%s')", filename, filepath));
        }

        public void getFile(String filepath, String to) {

            String hash = Hasher.digest(filepath);
            List<NodeInfo> dht = server.getReadAddressFor(hash);

            Status result;
            for (NodeInfo node : dht) {

                try {
                    result = DHTController.at(node.getIp(), node.getPort()).getFile(filepath, to);
                } catch (IOException e) { throw new RuntimeException(e); }

                if (result.equals(Status.HASH_NOT_FOUND)) {
                    logger.warn(String.format("file '%s' not found in dht node %s:%d, trying next node...", filepath, node.getIp(), node.getPort()));
                    continue;
                }

                if (result.equals(Status.IO_ERROR)) {
                    logger.error(String.format("get file '%s' failed, dht status code: %s", filepath, result));
                    return;
                }

                if (result.equals(Status.SUCCESS)) {
                    logger.info(String.format("successfully downloaded file at '%s' to '%s' from node %s:%d", filepath, to, node.getIp(), node.getPort()));
                    break;
                }
            }

            // todo: confirm this is correct
        }

        public void removeFile(String filename) {

            Operation<String> o = new Operation<>("removeFile", filename);
            Operation<File> crdtsOp = crdts.handleOperation(o);

            try { broadcaster.broadcast(crdtsOp); }
            catch (IOException e) { throw new RuntimeException(e); }

            logger.info(String.format("successfully removed file '%s'", filename));
        }

        public void addUser(String username) {
            Operation<String> o = new Operation<>("addUser", username);
            Operation<String> crdtsOp = crdts.handleOperation(o);

            try { broadcaster.broadcast(crdtsOp); }
            catch (IOException e) { throw new RuntimeException(e); }
        }

        public void removeUser(String username) {
            Operation<String> o = new Operation<>("removeUser", username);
            Operation<String> crdtsOp = crdts.handleOperation(o);

            try { broadcaster.broadcast(crdtsOp); }
            catch (IOException e) { throw new RuntimeException(e); }
        }

        public void rate(String username, String filename, String value) {

            if (!crdts.getVotersCRDT().canRate(username)) {
                logger.debug(String.format("blocked user '%s' from rating file '%s'", username, filename));
                return; // if user already voted, then ignore rate request for filename
            }

            int rateValue = Integer.parseInt(value);

            crdts.getFileRatingsCRDT().increment(filename, rateValue); // rate the file by incrementing the counter
            Operation<Rating> op = crdts.getVotersCRDT().addRating(filename, username, crdts.getFileRatingsCRDT().getValue(filename)); // set user as already voted in the GOSet

            try { broadcaster.broadcast(op); } catch (IOException e) { throw new RuntimeException(e); }
        }

        public void chat(String message) {
            try { broadcaster.broadcast(new Operation<>("chat", message)); }
            catch (IOException e) { throw new RuntimeException(e); }
        }

    }
}
