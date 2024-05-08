package org.client.crdts;

import org.client.crdts.base.VersionVector;
import org.messages.central.AlbumMessage;
import org.messages.central.Classification;
import org.messages.central.File;
import org.messages.p2p.OperationMessage;
import org.client.crdts.base.Operation;

import java.util.*;

@FunctionalInterface
interface OperationHandler {
    Operation handle(Operation op);
}

public class Album {

    private static Album instance = null;

    private GOSet fileRatingsCRDT = null;
    private ORset<File> filesCRDT = null;
    private ORset<String> usersCRDT = null;

    private String nodeId;

    private final Map<String, OperationHandler> operationHandlers = new HashMap<>();

    private Album() {
        instance = this;

        operationHandlers.put("addFile", (o) -> {
            return filesCRDT.addElement(o.operation, ((Operation<File>)o).element, nodeId);
        });

        operationHandlers.put("removeFile", (o) -> {
            return filesCRDT.removeElement(o.operation, ((Operation<File>)o).element);
        });

        operationHandlers.put("addUser", (o) -> {
            return usersCRDT.addElement(o.operation, ((Operation<String>)o).element, nodeId);
        });

        operationHandlers.put("removeUser", (o) -> {
            return usersCRDT.removeElement(o.operation, ((Operation<String>)o).element);
        });

        operationHandlers.put("rate", (o) -> {
            //return fileRatingsCRDT.applyAddRatingOperation(o);
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

    public Operation handleOperation(Operation op) {
        OperationMessage operationMessage = op.getOperationMessage();
        OperationHandler handler = this.operationHandlers.get(operationMessage.getOperation());
        return handler.handle(op);
    } // todo: this is not right!

    public void setFileRatingsCRDT(GOSet fileRatingsCRDT) {
        this.fileRatingsCRDT = fileRatingsCRDT;
    }

    public void setFilesCRDT(ORset filesCRDT) {
        this.filesCRDT = filesCRDT;
    }

    public void setUsers(List<String> users, String id){
        this.usersCRDT = new ORset();
        for (String user : users){
            this.usersCRDT.insert(user, new VersionVector(id, 0));
        }

        //this.usersCRDT.setUsers(users);
    }

    public void setFiles(List<File> files, String id){
        this.filesCRDT = new ORset();
        for (File file : files){
            this.filesCRDT.insert(file, new VersionVector(id, 0));
        }
    }

    public void setUsersCRDT(ORset usersCRDT) {
        this.usersCRDT = usersCRDT;
    }

    public GOSet getFileRatingsCRDT() {
        return fileRatingsCRDT;
    }

    public ORset getFilesCRDT() {
        return filesCRDT;
    }

    public ORset getUsersCRDT() {
        return usersCRDT;
    }

    public void setNodeId(String nodeId){
        this.nodeId = nodeId;
    }

    public String toString(){
        return this.usersCRDT.toString();
    }

    public AlbumMessage toAlbumMessage(){
        AlbumMessage.Builder b = AlbumMessage.newBuilder();
        b.addAllUsers(usersCRDT.elements());
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
}
