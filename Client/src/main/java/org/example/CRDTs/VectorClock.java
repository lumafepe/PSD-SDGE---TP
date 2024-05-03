package org.example.CRDTs;

import java.io.Serializable;

public class VectorClock implements Serializable {
    public String nodeId;
    public int counter;

    public VectorClock(String nodeId, int counter) {
        this.nodeId = nodeId;
        this.counter = counter;
    }
}
