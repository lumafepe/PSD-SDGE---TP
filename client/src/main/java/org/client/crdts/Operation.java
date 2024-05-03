package org.client.crdts;

import java.io.*;
import java.util.Set;

public class Operation implements Serializable{
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

    public String toString(){
        return operation + ", " + vectorClock + ", " + observed;
    }

    public byte[] serialize() {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();

        try (ObjectOutputStream out = new ObjectOutputStream(bos)) {
            out.writeObject(this);
            out.flush();
            return bos.toByteArray();
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

    public static Operation deserialize(byte[] bytes) {
        ByteArrayInputStream bis = new ByteArrayInputStream(bytes);

        try (ObjectInput in = new ObjectInputStream(bis)) {
            return (Operation) in.readObject();
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }
}
