package org.client.crdts.base;

import client.p2p.OperationMessage;

import java.io.*;
import java.util.HashSet;
import java.util.Set;

public class Operation implements Serializable {
    public String operation;
    public String element;
    public VersionVector versionVector;
    public Set<VersionVector> observed;
    // For ratings
    public String user;
    public String fileName;
    public int rating;

    // For Orset
    public Operation(String operation, String element, VersionVector versionVector, Set<VersionVector> observed) {
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
    public Operation(String operation, String message) {
        this.operation = operation;
        this.element = message;
    }

    public Operation(OperationMessage om) {
        if (om.hasOperation())
            this.operation = om.getOperation();
        if (om.hasElement())
            this.element = om.getElement();

        if (om.hasVectorClock())
            this.versionVector = new VersionVector(om.getVectorClock().getNodeId(), om.getVectorClock().getCounter());

        this.observed = new HashSet<>();
        for (client.p2p.VectorClock vectorClock : om.getObservedList()) {
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
            builder.setElement(element);

        if (this.versionVector != null) {
            builder.setVectorClock(client.p2p.VectorClock.newBuilder()
                    .setCounter(this.versionVector.counter)
                    .setNodeId(this.versionVector.nodeId)
                    .build());
        }

        if (observed != null) {
            for (VersionVector versionVector : observed) {
                builder.addObserved(client.p2p.VectorClock.newBuilder()
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
        element = parts[1];
        observed = new HashSet<>();

    }

}
