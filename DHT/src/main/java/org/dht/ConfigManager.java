package org.dht;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;

public class ConfigManager {
    private static ConfigManager instance = null;

    private int port;
    private String baseDirectory;

    private int tokenCount;

    private long mod;

    private ConfigManager() {
        instance = this;
    }

    public static ConfigManager getInstance() {
        if(instance == null) {
            instance = new ConfigManager();
        }

        return instance;
    }

    public int getPort() {
        return this.port;
    }

    public String getBaseDirectory() {
        return this.baseDirectory;
    }

    public int getTokenCount() {
        return tokenCount;
    }

    public long getMod() {
        return mod;
    }

    //TODO: Use config .yml file
    public void parseConfig(String[] args) throws IllegalArgumentException {
        if(args.length % 2 == 1)
            throw new IllegalArgumentException("Number of arguments must be odd");

        for(int i = 0; i < args.length; i += 2) {
            if(args[i].equals("-p")) {
                this.port = Integer.parseInt(args[i + 1]);
            } else if(args[i].equals("-b")) {
                this.baseDirectory = args[i + 1];
            } else if(args[i].equals("-t")) {
                this.tokenCount = Integer.parseInt(args[i + 1]);
            } else if(args[i].equals("-m")) {
                this.mod = Long.parseLong(args[i + 1]);
            } else {
                throw new IllegalArgumentException("Unknown argument " + args[i]);
            }
        }

        checkArguments();
    }

    private void checkArguments() throws IllegalArgumentException {
        checkPort();
        checkBaseDirectory();
        checkTokenCountAndMod();
    }

    private void checkPort() throws IllegalArgumentException {
        if(this.port <= 0 || this.port >= 65535) {
            throw new IllegalArgumentException("Port value must be between 0 and 65535");
        }
    }

    private void checkBaseDirectory() throws IllegalArgumentException {
        File dir = new File(this.baseDirectory);
        if(!dir.isDirectory())
            throw new IllegalArgumentException("Base directory must be a directory and exist");

        if(!this.baseDirectory.endsWith("/"))
            this.baseDirectory = this.baseDirectory + "/";
    }

    private void checkTokenCountAndMod() throws IllegalArgumentException {
        if(this.tokenCount <= 0)
            throw new IllegalArgumentException("Token count must be positive");

        if(this.mod <= 0)
            throw new IllegalArgumentException("Mod must be positive");
    }
}
