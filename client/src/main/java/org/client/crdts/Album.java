package org.client.crdts;

import org.client.crdts.base.VersionVector;
import org.client.crdts.records.FileRecord;
import org.client.crdts.records.Rating;
import org.messages.central.AlbumMessage;
import org.messages.central.File;
import org.messages.p2p.OperationMessage;
import org.client.crdts.base.Operation;

import java.io.*;
import java.util.*;

@FunctionalInterface
interface OperationHandler<T> {
    Operation<T> handle(Operation<T> op);
}

public class Album {

    private static Album instance = null;

    private CRDTS crdts = new CRDTS();

    private String nodeId;

    private final Map<String, OperationHandler> operationHandlers = new HashMap<>();

    private Album() {
        instance = this;

        operationHandlers.put("addFile", (o) -> {
            if (o.element.getClass().equals(FileRecord.class)) {
                return crdts.filesCRDT.addElement(o.operation, (FileRecord) o.element, nodeId);
            }
            else return null;
        });

        operationHandlers.put("removeFile", (o) -> o.element instanceof FileRecord ? crdts.filesCRDT.removeElement(o.operation, (FileRecord) o.element) : null);
        operationHandlers.put("addUser", (o) -> o.element instanceof String ? crdts.usersCRDT.addElement(o.operation, (String) o.element, nodeId) : null);
        operationHandlers.put("removeUser", (o) -> o.element instanceof String ? crdts.usersCRDT.removeElement(o.operation, (String) o.element) : null);

        operationHandlers.put("rate", (o) -> {
            Rating r = new Rating(o.user, o.fileName, o.rating);
            if (!crdts.fileVotersCRDT.containsKey(r.fileName())) {
                crdts.fileVotersCRDT.put(r.fileName(), new GOSet());
            }
            crdts.fileVotersCRDT.get(r.fileName()).applyAddRatingOperation(o);
            crdts.fileRatingsCRDT.applyIncrementOperation(r);

            return null;
        });

        operationHandlers.put("chat", (o) -> {
            System.out.println("\n[home][chat] " + o.element);
            return null;
        });
    }

    public static Album getInstance() {
        if (instance == null) {
            instance = new Album();
        }
        return instance;
    }

    public Operation handleOperation(Operation op) {
        OperationMessage operationMessage = op.getOperationMessage();
        OperationHandler handler = this.operationHandlers.get(operationMessage.getOperation());
        return handler.handle(op);
    }

    public void setFileRatingsCRDT(GCounter fileRatingsCRDT) {
        this.crdts.fileRatingsCRDT = fileRatingsCRDT;
    }

    public void setFilesCRDT(ORset<FileRecord> filesCRDT) {
        this.crdts.filesCRDT = filesCRDT;
    }

    public void setUsers(List<String> users, String id) {
        this.crdts.usersCRDT = new ORset<>();
        for (String user : users) {
            this.crdts.usersCRDT.insert(user, new VersionVector(id, 0));
        }
        // this.usersCRDT.setUsers(users);
    }

    public void setFiles(List<FileRecord> files, String id) {
        this.crdts.filesCRDT = new ORset<>();
        for (FileRecord file : files) {
            this.crdts.filesCRDT.insert(file, new VersionVector(id, 0));
        }
    }

    public void setVotersCRDT(Map<String, GOSet> votersCRDT) {
        this.crdts.fileVotersCRDT = votersCRDT;
    }

    public void setUsersCRDT(ORset<String> usersCRDT) {
        this.crdts.usersCRDT = usersCRDT;
    }

    public Map<String, GOSet> getVotersCRDT() {
        return this.crdts.fileVotersCRDT;
    }

    public GCounter getFileRatingsCRDT() {
        return crdts.fileRatingsCRDT;
    }

    public ORset<FileRecord> getFilesCRDT() {
        return crdts.filesCRDT;
    }

    public ORset<String> getUsersCRDT() {
        return crdts.usersCRDT;
    }

    public void setCrdts(CRDTS crdts) {
        this.crdts = crdts;
    }

    public CRDTS getCrdts() {
        return crdts;
    }

    public void setNodeId(String nodeId) {
        this.nodeId = nodeId;
    }

    public String toString() {
        return this.crdts.usersCRDT.toString();
    }

    public AlbumMessage toAlbumMessage() {
        AlbumMessage.Builder builder = AlbumMessage.newBuilder();
        builder.addAllUsers(crdts.usersCRDT.elements());

        ArrayList<File> files = new ArrayList<>();
        for (FileRecord f : crdts.filesCRDT.elements()) {

            GOSet votes = crdts.fileVotersCRDT.get(f.name());

            File file = File.newBuilder()
                            .setName(f.name())
                            .setHash(f.hash())
                            .addAllClassifications(votes != null ? votes.getFileRatings() : new ArrayList<>())
                            .build();

            files.add(file);
        }

        builder.addAllFiles(files);
        return builder.build();
    }

    public byte[] asBytes() throws IOException {

        ByteArrayOutputStream byteArray = new ByteArrayOutputStream();
        ObjectOutputStream bytes = new ObjectOutputStream(byteArray);

        bytes.writeObject(this.crdts);
        return byteArray.toByteArray();
    }

    public static CRDTS fromBytes(byte[] payload) throws IOException, ClassNotFoundException {

        ByteArrayInputStream byteIn = new ByteArrayInputStream(payload);
        ObjectInputStream bytes = new ObjectInputStream(byteIn);
        return (CRDTS) bytes.readObject();
    }
}
