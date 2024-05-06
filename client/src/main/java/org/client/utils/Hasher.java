package org.client.utils;

import java.io.FileInputStream;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class Hasher {

    public static final String HASH_MODE = "SHA-1";
    public static final int BLOCK_SIZE = 4096;

    public static String digest(String filepath) {

        String hash = null;
        try (FileInputStream inputStream = new FileInputStream(filepath)) {

            MessageDigest hashSum = MessageDigest.getInstance(HASH_MODE);

            byte[] buffer = new byte[BLOCK_SIZE];

            int read;
            while ((read = inputStream.read(buffer)) != -1) {
                hashSum.update(buffer, 0, read);
            }

            byte[] hashBytes = hashSum.digest();
            hash = bytesToHex(hashBytes);

        } catch (IOException e) {
            throw new RuntimeException("error reading file '" + filepath + "'", e);
        }
        catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("algorithm '" + filepath + "' does not exist", e);
        }

        return hash;
    }

    private static String bytesToHex(byte[] hash) {

        StringBuilder hexString = new StringBuilder(2 * hash.length);
        for (byte b : hash) {
            String hex = Integer.toHexString(0xff & b);
            if (hex.length() == 1) {
                hexString.append('0');
            }
            hexString.append(hex);
        }
        return hexString.toString();
    }
}
