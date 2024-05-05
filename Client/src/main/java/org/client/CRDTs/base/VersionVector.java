package org.client.CRDTs.base;

import java.io.Serializable;

public class VersionVector implements Serializable {
    public String nodeId;
    public int counter;

    public VersionVector(String nodeId, int counter) {
        this.nodeId = nodeId;
        this.counter = counter;
    }
}
