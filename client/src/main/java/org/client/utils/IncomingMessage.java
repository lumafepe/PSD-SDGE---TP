package org.client.utils;

public record IncomingMessage(byte[] identity, byte[] data) {
}