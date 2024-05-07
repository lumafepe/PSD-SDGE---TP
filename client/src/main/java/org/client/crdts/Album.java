package org.client.crdts;

import org.client.crdts.base.VersionVector;
import org.messages.central.File;
import org.messages.p2p.OperationMessage;
import org.client.crdts.base.Operation;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@FunctionalInterface
interface OperationHandler {
    Operation handle(Operation op);
}

public class Album {

    private static Album instance = null;

    private GOSet fileRatingsCRDT = null;
    private ORset filesCRDT = null;
    private ORset usersCRDT = null;

    private String nodeId;

    private final Map<String, OperationHandler> operationHandlers = new HashMap<>();

    private Album() {
        instance = this;

        operationHandlers.put("addFile", (o) -> {
            return filesCRDT.addElement(o.operation, o.element, nodeId);
        });

        operationHandlers.put("removeFile", (o) -> {
            return filesCRDT.removeElement(o.operation, o.element);
        });

        operationHandlers.put("addUser", (o) -> {
            return usersCRDT.addElement(o.operation, o.element, nodeId);
        });

        operationHandlers.put("removeUser", (o) -> {
            return usersCRDT.removeElement(o.operation, o.element);
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
}
