package org.dht.config;

import org.dht.exceptions.InvalidConfigurationFileException;
import org.yaml.snakeyaml.Yaml;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

public class ConfigManager {
    private static ConfigManager instance = null;

    private Config config;

    private ConfigManager() {
        instance = this;
    }

    public static ConfigManager getInstance() {
        if (instance == null) {
            instance = new ConfigManager();
        }
        return instance;
    }

    public void load(String filepath) throws InvalidConfigurationFileException, IOException {

        Yaml yaml = new Yaml();

        File file = new File(filepath);
        if (!file.isFile()) {
            throw new IllegalArgumentException("file '" + filepath + "' does not exist");
        }

        try (FileInputStream f = new FileInputStream(file)) {
            this.config = yaml.loadAs(f, Config.class);
        }

        this.config.getDht().validate();
        this.config.getServer().validate();
        // ... add more if needed
    }

    public Config getConfig() {
        return config;
    }
}
