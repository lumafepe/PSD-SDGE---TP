package org.dht.config;

import java.io.File;

public class DHTConfig {
    private int port;
    private String baseDirectory;

    public int getPort() {
        return port;
    }

    public String getBaseDirectory() {
        return baseDirectory;
    }

    public void validate() throws RuntimeException {

        if (this.port < 0 || this.port > 65535) {
            throw new RuntimeException("'port' (" + this.port + ") value must be between 0 and 65535");
        }

        File dir = new File(this.baseDirectory);
        if (!dir.isDirectory()) {
            throw new RuntimeException("'baseDirectory' ('" + this.baseDirectory + "') must be an existing directory");
        }

        if (!this.baseDirectory.endsWith("/")) {
            this.baseDirectory = this.baseDirectory + "/";
        }
    }
}
