package org.example;

import java.util.Set;

public class Operation {
    public String operation;
    public Object element;
    public VectorClock vectorClock;
    public Set<VectorClock> observed;

    public Operation(String operation, Object element, VectorClock vectorClock, Set<VectorClock> observed) {
        this.operation = operation;
        this.element = element;
        this.vectorClock = vectorClock;
        this.observed = observed;
    }
}
