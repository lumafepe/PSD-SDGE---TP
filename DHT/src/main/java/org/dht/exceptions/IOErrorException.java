package org.dht.exceptions;

import java.io.IOException;

public class IOErrorException extends IOException {
    public IOErrorException(Exception baseException) {
        super(baseException);
    }
}
