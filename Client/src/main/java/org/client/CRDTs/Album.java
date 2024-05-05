package org.client.CRDTs;

import client.p2p.OperationMessage;
import com.google.protobuf.InvalidProtocolBufferException;
import org.client.CRDTs.base.Operation;

import java.util.HashMap;
import java.util.Map;

@FunctionalInterface
interface OperationHandler {
    void handle(Operation op);
}

public class Album {

    private static Album instance = null;

    private GOSet fileRatingsCRDT = null;
    private ORset filesCRDT = null;
    private ORset usersCRDT = null;

    private final Map<String, OperationHandler> operationHandlers = new HashMap<>();

    private Album() {
        instance = this;

        operationHandlers.put("addFile", (o) -> {
            filesCRDT.applyAddOperation(o);
        });

        operationHandlers.put("removeFile", (o) -> {
            filesCRDT.applyRemoveOperation(o);
        });

        operationHandlers.put("addUser", (o) -> {
            usersCRDT.applyAddOperation(o);
        });

        operationHandlers.put("removeUser", (o) -> {
            usersCRDT.applyRemoveOperation(o);
        });

        operationHandlers.put("rate", (o) -> {
            fileRatingsCRDT.applyAddRatingOperation(o);
        });

        operationHandlers.put("chat", (o) -> {
            System.out.println("New message: " + o.element);
        });
    }

    public static Album getInstance() {
        if (instance == null) {
            instance = new Album();
        }
        return instance;
    }

    public void handleOperation(Operation op) {
        OperationMessage operationMessage = op.getOperationMessage();
        OperationHandler handler = this.operationHandlers.get(operationMessage.getOperation());
        handler.handle(op);
    } // todo: this is not right!

    public void setFileRatingsCRDT(GOSet fileRatingsCRDT) {
        this.fileRatingsCRDT = fileRatingsCRDT;
    }

    public void setFilesCRDT(ORset filesCRDT) {
        this.filesCRDT = filesCRDT;
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
}
