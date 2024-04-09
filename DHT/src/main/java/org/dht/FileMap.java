package org.dht;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.file.NoSuchFileException;
import java.util.HashMap;
import java.util.Map;

public class FileMap {
    private String baseDirectory;

    public FileMap(String directory) {
        this.baseDirectory = directory + "/";
    }

    public boolean read(String hash, long offset, byte[] buffer, int count) throws FileNotFoundException, IOException {
        try (RandomAccessFile file = new RandomAccessFile(this.baseDirectory + hash, "r")) {
            file.seek(offset);
            int result = file.read(buffer);

            return result > 0;
        }
    }

    public void write(String hash, long offset, byte[] data) throws FileNotFoundException, IOException {
        try (RandomAccessFile file = new RandomAccessFile(this.baseDirectory + hash, "rw")) {
            file.seek(offset);
            file.write(data);
        }
    }
}
