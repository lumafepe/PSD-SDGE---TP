package org.example.CRDTs;

import java.io.*;
import java.util.HashSet;
import java.util.Set;

public class Operation implements Serializable{
    public String operation;
    public String element;
    public VectorClock vectorClock;
    public Set<VectorClock> observed;

    public Operation(String operation, String element, VectorClock vectorClock, Set<VectorClock> observed) {
        this.operation = operation;
        this.element = element;
        this.vectorClock = vectorClock;
        this.observed = observed;
    }

    public String toString(){
        return operation + ", " + element + ", " + vectorClock.toString() + ", " + observed;
    }

    public void fromString(String s){
        String[] parts = s.split(",");
        operation = parts[0];
        element = parts[1];
        observed = new HashSet<>();

    }

}
