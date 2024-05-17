package org.client.messages;

import org.client.crdts.base.Operation;
import org.client.utils.VectorClock;

import java.io.*;

public record BroadcastMessage(int index, VectorClock version, Operation operation) implements Serializable {

    public byte[] asBytes() throws IOException {

        ByteArrayOutputStream byteArray = new ByteArrayOutputStream();
        ObjectOutputStream bytes = new ObjectOutputStream(byteArray);

        bytes.writeObject(this);
        return byteArray.toByteArray();
    }

    public static BroadcastMessage fromBytes(byte[] payload) throws IOException, ClassNotFoundException {

        ByteArrayInputStream byteIn = new ByteArrayInputStream(payload);
        ObjectInputStream bytes = new ObjectInputStream(byteIn);
        return (BroadcastMessage) bytes.readObject();
    }

}
