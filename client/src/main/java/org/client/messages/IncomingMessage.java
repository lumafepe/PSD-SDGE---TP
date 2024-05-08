package org.client.messages;

public record IncomingMessage(byte[] identity, byte[] data) {
}
