package org.dht.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;

import java.io.File;
import java.io.IOException;

public class Manager {
    private static Manager instance = null;

    private DHTConfig dht;

    private Manager() {
        instance = this;
    }

    public static Manager getInstance() {
        if (instance == null) {
            instance = new Manager();
        }
        return instance;
    }

    public void load(String filepath) throws RuntimeException, IOException {

        ObjectMapper mapper = new ObjectMapper(new YAMLFactory());

        File configFile = new File(filepath);
        if (!configFile.isFile()) {
            throw new RuntimeException("file path '" + filepath + "' does not exists");
        }

        DHTConfig config = mapper.readValue(new File(filepath), DHTConfig.class);
        config.validate();

        this.dht = config;
    }

    public DHTConfig getDHT() {
        return dht;
    }
}
