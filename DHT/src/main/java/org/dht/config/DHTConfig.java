package org.dht.config;

import org.dht.exceptions.InvalidConfigurationFileException;

import java.io.File;

public class DHTConfig {

    private int port;
    private String baseDirectory;
    private int tokenCount;
    private long mod;

    public int getPort() {
        return port;
    }

    public String getBaseDirectory() {
        return baseDirectory;
    }

    public long getMod() {
        return mod;
    }

    public int getTokenCount() {
        return tokenCount;
    }

    public void setPort(int port) {
        this.port = port;
    }

    public void setBaseDirectory(String baseDirectory) {
        this.baseDirectory = baseDirectory;
    }

    public void setTokenCount(int tokenCount) {
        this.tokenCount = tokenCount;
    }

    public void setMod(long mod) {
        this.mod = mod;
    }

    public void validate() throws InvalidConfigurationFileException {

        if (this.port < 0 || this.port > 65535) {
            throw new InvalidConfigurationFileException("dht.port '" + this.port + "' value must be between 0 and 65535");
        }

        File dir = new File(this.baseDirectory);
        if (!dir.isDirectory()) {
            throw new InvalidConfigurationFileException("dht.base_directory '" + this.baseDirectory + "' must be an existing directory");
        }

        if (!this.baseDirectory.endsWith("/")) {
            this.baseDirectory = this.baseDirectory + "/";
        }

        if (this.tokenCount <= 0) {
            throw new InvalidConfigurationFileException("dht.token_count '" + this.tokenCount + "' must be a positive integer");
        }

        if (this.mod <= 0) {
            throw new InvalidConfigurationFileException("dht.mod '" + this.mod + "' must be a positive integer");
        }

    }
}
