package org.client.messages;

import org.client.crdts.CRDTS;

import java.io.*;

public record ClientMessage (String type, BroadcastMessage message, CRDTS crdts, String identity) implements Serializable {
    public byte[] asBytes() throws IOException {

        ByteArrayOutputStream byteArray = new ByteArrayOutputStream();
        ObjectOutputStream bytes = new ObjectOutputStream(byteArray);

        bytes.writeObject(this);
        return byteArray.toByteArray();
    }

    public static ClientMessage fromBytes(byte[] payload) throws IOException, ClassNotFoundException {

        ByteArrayInputStream byteIn = new ByteArrayInputStream(payload);
        ObjectInputStream bytes = new ObjectInputStream(byteIn);
        return (ClientMessage) bytes.readObject();
    }
}
