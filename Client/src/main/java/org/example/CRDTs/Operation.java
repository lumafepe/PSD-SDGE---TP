package org.example.CRDTs;

import client.p2p.Client;
import client.p2p.OperationMessage;

import java.io.*;
import java.util.HashSet;
import java.util.Set;

public class Operation implements Serializable{
    public String operation;
    public String element;
    public VectorClock vectorClock;
    public Set<VectorClock> observed;
    // For ratings
    public String user;
    public String fileName;
    public int rating;

    public Operation(String operation, String element, VectorClock vectorClock, Set<VectorClock> observed) {
        this.operation = operation;
        this.element = element;
        this.vectorClock = vectorClock;
        this.observed = observed;
    }

    public Operation(String operation, String user, String fileName, int rating) {
        this.operation = operation;
        this.user = user;
        this.fileName = fileName;
        this.rating = rating;
    }

    public Operation(OperationMessage om) {
        if (om.hasOperation())
            this.operation = om.getOperation();
        if (om.hasElement())
            this.element = om.getElement();

        if (om.hasVectorClock())
            this.vectorClock = new VectorClock(om.getVectorClock().getNodeId(), om.getVectorClock().getCounter());

        this.observed = new HashSet<>();
        for (client.p2p.VectorClock vectorClock : om.getObservedList()) {
            this.observed.add(new VectorClock(vectorClock.getNodeId(), (int)vectorClock.getCounter()));
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

        if (this.vectorClock != null) {
            builder.setVectorClock(client.p2p.VectorClock.newBuilder()
                    .setCounter(this.vectorClock.counter)
                    .setNodeId(this.vectorClock.nodeId)
                    .build());
        }

        if (observed != null) {
            for (VectorClock vectorClock : observed) {
                builder.addObserved(client.p2p.VectorClock.newBuilder()
                        .setNodeId(vectorClock.nodeId)
                        .setCounter(vectorClock.counter)
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
        return operation + ", " + element + ", " + vectorClock + ", " + observed;
    }

    public void fromString(String s){
        String[] parts = s.split(",");
        operation = parts[0];
        element = parts[1];
        observed = new HashSet<>();

    }

}
