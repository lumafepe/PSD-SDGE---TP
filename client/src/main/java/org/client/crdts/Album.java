package org.client.crdts;

import org.client.crdts.base.VersionVector;
import org.client.crdts.records.Rating;
import org.messages.central.AlbumMessage;
import org.messages.central.Classification;
import org.messages.central.File;
import org.messages.p2p.OperationMessage;
import org.client.crdts.base.Operation;

import java.io.*;
import java.util.*;

@FunctionalInterface
interface OperationHandler {
    Operation handle(Operation op);
}

public class Album {

    private static Album instance = null;

    private CRDTS crdts = new CRDTS();

    private String nodeId;

    private final Map<String, OperationHandler> operationHandlers = new HashMap<>();

    private Album() {
        instance = this;

        operationHandlers.put("addFile", (o) -> crdts.filesCRDT.addElement(o.operation, ((Operation<File>) o).element, nodeId));
        operationHandlers.put("removeFile", (o) -> crdts.filesCRDT.removeElement(o.operation, ((Operation<File>) o).element));
        operationHandlers.put("addUser", (o) -> crdts.usersCRDT.addElement(o.operation, ((Operation<String>) o).element, nodeId));
        operationHandlers.put("removeUser", (o) -> crdts.usersCRDT.removeElement(o.operation, ((Operation<String>) o).element));

        operationHandlers.put("rate", (o) -> {
            crdts.fileRatingsCRDT.applyIncrementOperation((Operation<Rating>) o);
            return null;
        });

        operationHandlers.put("chat", (o) -> {
            System.out.println("New message: " + o.element);
            return null;
        });
    }

    public static Album getInstance() {
        if (instance == null) {
            instance = new Album();
        }
        return instance;
    }

    public Operation<?> handleOperation(Operation<?> op) {
        OperationMessage operationMessage = op.getOperationMessage();
        OperationHandler handler = this.operationHandlers.get(operationMessage.getOperation());
        return handler.handle(op);
    }

    public void setFileRatingsCRDT(GCounter fileRatingsCRDT) {
        this.crdts.fileRatingsCRDT = fileRatingsCRDT;
    }

    public void setFilesCRDT(ORset<File> filesCRDT) {
        this.crdts.filesCRDT = filesCRDT;
    }

    public void setUsers(List<String> users, String id){
        this.crdts.usersCRDT = new ORset<String>();
        for (String user : users) {
            this.crdts.usersCRDT.insert(user, new VersionVector(id, 0));
        }
        // this.usersCRDT.setUsers(users);
    }

    public void setFiles(List<File> files, String id){
        this.crdts.filesCRDT = new ORset<File>();
        for (File file : files){
            this.crdts.filesCRDT.insert(file, new VersionVector(id, 0));
        }
    }

    public void setVotersCRDT(GOSet votersCRDT) {
        this.crdts.fileVotersCRDT = votersCRDT;
    }

    public void setUsersCRDT(ORset<String> usersCRDT) {
        this.crdts.usersCRDT = usersCRDT;
    }

    public GOSet getVotersCRDT() {
        return this.crdts.fileVotersCRDT;
    }

    public GCounter getFileRatingsCRDT() {
        return crdts.fileRatingsCRDT;
    }

    public ORset<File> getFilesCRDT() {
        return crdts.filesCRDT;
    }

    public ORset<String> getUsersCRDT() {
        return crdts.usersCRDT;
    }

    public void setCrdts(CRDTS crdts){
        this.crdts = crdts;
    }

    public CRDTS getCrdts(){
        return crdts;
    }

    public void setNodeId(String nodeId){
        this.nodeId = nodeId;
    }

    public String toString(){
        return this.crdts.usersCRDT.toString();
    }

    public AlbumMessage toAlbumMessage(){
        AlbumMessage.Builder b = AlbumMessage.newBuilder();
        b.addAllUsers(crdts.usersCRDT.elements());
        ArrayList<File> files = new ArrayList<>();
        ArrayList<Classification> classifications = new ArrayList<>();
        classifications.add(Classification.newBuilder().setUsername("miguel").setValue(5).build());
        files.add(File.newBuilder()
                .setName("file1")
                .setHash(123)
                .addAllClassifications(classifications)
                .build());
        b.addAllFiles(files);
        return b.build();
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
