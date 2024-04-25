package org.example.CRDTs;

public class VectorClock {
    public long nodeId;
    public int counter;

    public VectorClock(long node_id, int counter) {
        this.nodeId = node_id;
        this.counter = counter;
    }
}
