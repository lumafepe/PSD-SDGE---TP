package org.client.crdts.base;

import org.messages.p2p.OperationMessage;
import org.messages.p2p.VectorClock;

import java.io.*;
import java.util.HashSet;
import java.util.Set;

public class Operation<T> implements Serializable {
    public String operation;
    public T element;
    public VersionVector versionVector;
    public Set<VersionVector> observed;
    // For ratings
    public String user;
    public String fileName;
    public int rating;

    // For Orset
    public Operation(String operation, T element, VersionVector versionVector, Set<VersionVector> observed) {
        this.operation = operation;
        this.element = element;
        this.versionVector = versionVector;
        this.observed = observed;
    }

    // For ratings
    public Operation(String operation, String user, String fileName, int rating) {
        this.operation = operation;
        this.user = user;
        this.fileName = fileName;
        this.rating = rating;
    }

    // For chat
    public Operation(String operation, T element) {
        this.operation = operation;
        this.element = element;
    }

    public Operation(OperationMessage om) {
        if (om.hasOperation())
            this.operation = om.getOperation();
        if (om.hasElement())
            this.element = (T) om.getElement();

        if (om.hasVectorClock())
            this.versionVector = new VersionVector(om.getVectorClock().getNodeId(), om.getVectorClock().getCounter());

        this.observed = new HashSet<>();
        for (VectorClock vectorClock : om.getObservedList()) {
            this.observed.add(new VersionVector(vectorClock.getNodeId(), (int)vectorClock.getCounter()));
        }

        if (om.hasUser())
            this.user = om.getUser();

        if (om.hasFileName())
            this.fileName = om.getFileName();

        if (om.hasRating())
            this.rating = om.getRating();
    }

    public OperationMessage getOperationMessage() {
        OperationMessage.Builder builder = OperationMessage.newBuilder();

        if (this.operation != null)
            builder.setOperation(operation);

        if (this.element != null)
            builder.setElement(element.toString());

        if (this.versionVector != null) {
            builder.setVectorClock(VectorClock.newBuilder()
                    .setCounter(this.versionVector.counter)
                    .setNodeId(this.versionVector.nodeId)
                    .build());
        }

        if (observed != null) {
            for (VersionVector versionVector : observed) {
                builder.addObserved(VectorClock.newBuilder()
                        .setNodeId(versionVector.nodeId)
                        .setCounter(versionVector.counter)
                        .build());
            }
        }

        if (this.user != null)
            builder.setUser(user);

        if (this.fileName != null)
            builder.setFileName(fileName);

        if (this.rating != 0)
            builder.setRating(rating);

        return builder.build();
    }

    public String toString(){
        return operation + ", " + element + ", " + versionVector + ", " + observed;
    }

    public void fromString(String s){
        String[] parts = s.split(",");
        operation = parts[0];
        element = (T) parts[1];
        observed = new HashSet<>();

    }

}
