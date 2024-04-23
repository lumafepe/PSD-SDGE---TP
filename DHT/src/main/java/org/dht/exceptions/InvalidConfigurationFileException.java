package org.dht.exceptions;

public class InvalidConfigurationFileException extends Exception {
    public InvalidConfigurationFileException(String message) {
        super(message);
    }

    public InvalidConfigurationFileException(String message, Throwable cause) {
        super(message, cause);
    }
}
