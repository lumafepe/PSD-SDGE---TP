package org.client.controllers.peers;

import dht.messages.Status;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.client.controllers.DHTController;
import org.client.controllers.server.ServerController;
import org.client.crdts.GOSet;
import org.client.crdts.records.FileRecord;
import org.client.crdts.records.Rating;
import org.client.network.Broadcaster;
import org.client.messages.ClientMessage;
import org.client.crdts.Album;
import org.client.crdts.base.Operation;
import org.client.utils.Hasher;
import org.messages.central.*;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

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

    public Message handle(String data) {
        Message m = null;
        if (data.startsWith("/addFile")) {
            String[] split = data.substring("/addFile".length()).split(" ");
            m = Message.newBuilder().setType(Type.SUCESIUM).build();
            handlers.addFile(split[1], split[2]);
                             //^filename //^filepath
        }

        if (data.startsWith("/getFile")) {
            String[] split = data.substring("/getFile".length()).split(" ");
            m = Message.newBuilder().setType(Type.SUCESIUM).build();
            handlers.getFile(split[1], split[2]);
                             //^filename //^destination
        }

        if (data.startsWith("/removeFile")) {
            String[] split = data.substring("/removeFile".length()).split(" ");
            m = Message.newBuilder().setType(Type.SUCESIUM).build();
            handlers.removeFile(split[1]);
                                //^filename
        }

        if (data.startsWith("/addUser")) {
            String username = data.substring("/addUser ".length());
            m = Message.newBuilder().setType(Type.SUCESIUM).build();
            handlers.addUser(username);
        }

        if (data.startsWith("/removeUser")) {
            String username = data.substring("/removeUser ".length());
            m = Message.newBuilder().setType(Type.SUCESIUM).build();
            handlers.removeUser(username);
        }

        if (data.startsWith("/showAlbum")) {
            m = Message.newBuilder().setType(Type.ALBUM).setAlbum(this.crdts.toAlbumMessage()).build();
        }

        if (data.startsWith("/chat")) {
            String message = data.substring("/chat ".length());
            m = Message.newBuilder().setType(Type.SUCESIUM).build();
            handlers.chat(message);
        }

        if (data.startsWith("/rate")) {
            String[] split = data.substring("/rate".length()).split(" ");
            m = Message.newBuilder().setType(Type.SUCESIUM).build();
            handlers.rate(split[1], split[2], split[3]);
                          //^username //^filename //^value
        }
        return m;
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

        List<String> exclude = Arrays.asList("join", "forward", "state", "informJoin", "leave", "leaveAck", "leaveConfirm");
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
            NodeIp dht = server.getWriteAddressFor(hash).getFirst(); // get dht node ip:port

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

            Operation<FileRecord> o = new Operation<>("addFile", new FileRecord(filename, hash));
            Operation<FileRecord> crdtsOp = crdts.handleOperation(o);

            try { broadcaster.broadcast(crdtsOp); }
            catch (IOException e) { throw new RuntimeException(e); }

            logger.info(String.format("successfully added file '%s' ('%s')", filename, filepath));
        }

        private String getHashFromFilename(String filename) {
            for (FileRecord file : crdts.getFilesCRDT().elements()) {
                if (file.name().equals(filename)) {
                    return file.hash();
                }
            }
            return null;
        }

        public void getFile(String filename, String to) {

            String hash = this.getHashFromFilename(filename);
            if (hash == null) {
                throw new RuntimeException(String.format("file '%s' does not exist within this album\n", filename));
            }

            List<NodeIp> dht = server.getReadAddressFor(hash);

            Status result;
            for (NodeIp node : dht) {

                try {
                    result = DHTController.at(node.getIp(), node.getPort()).getFile(hash, to);
                } catch (IOException e) { throw new RuntimeException(e); }

                if (result.equals(Status.HASH_NOT_FOUND)) {
                    logger.warn(String.format("file not found in dht node %s:%d, trying next node...", node.getIp(), node.getPort()));
                    continue;
                }

                if (result.equals(Status.IO_ERROR)) {
                    logger.error(String.format("get file failed, dht status code: %s", result));
                    return;
                }

                if (result.equals(Status.SUCCESS)) {
                    logger.info(String.format("successfully downloaded file to '%s' from node %s:%d", to, node.getIp(), node.getPort()));
                    break;
                }
            }
        }

        public void removeFile(String filename) {
            FileRecord fr = new FileRecord(filename, this.getHashFromFilename(filename));
            Operation<FileRecord> o = new Operation<>("removeFile", fr);
            Operation<FileRecord> crdtsOp = crdts.handleOperation(o);

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

            if (!crdts.getVotersCRDT().containsKey(filename)) {
                logger.debug(String.format("added new entry on voters crdt for filename '%s'", filename));
                crdts.getVotersCRDT().put(filename, new GOSet()); // insert new goset for this filename
            }

            GOSet votes = crdts.getVotersCRDT().get(filename);
            if (!votes.canRate(username)) {
                logger.debug(String.format("blocked user '%s' from rating file '%s'", username, filename));
                return; // if user already voted, then ignore rate request for filename
            }

            int rateValue = Integer.parseInt(value);
            crdts.getFileRatingsCRDT().increment(filename, rateValue); // rate the file by incrementing the counter
            Operation<Rating> op = votes.addRating(filename, username, rateValue); // set user as already voted in the GOSet

            try { broadcaster.broadcast(op); } catch (IOException e) { throw new RuntimeException(e); }
        }

        public void chat(String message) {
            try { broadcaster.broadcast(new Operation<>("chat", message)); }
            catch (IOException e) { throw new RuntimeException(e); }
        }

    }
}
