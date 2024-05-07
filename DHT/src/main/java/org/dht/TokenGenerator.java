package org.dht;

import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class TokenGenerator {
    public static List<Long> generateTokens(int count, long mod) {
        List<Long> result = new ArrayList<>();

        for(int i = 0; i < count; i++) {
            String sha1Hash = calculateSHA1(generateRandomData());
            result.add(hashToRing(sha1Hash, mod));
        }

        Collections.sort(result);
        return result;
    }

    public static long hashToRing(String hash, long mod) {
        BigInteger integer = new BigInteger(hash, 16);
        return integer.mod(BigInteger.valueOf(mod)).longValue();
    }

    private static String calculateSHA1(String data) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-1");
            byte[] hash = digest.digest(data.getBytes());
            StringBuilder hexString = new StringBuilder();
            for (byte b : hash) {
                String hex = Integer.toHexString(0xff & b);
                if (hex.length() == 1) hexString.append('0');
                hexString.append(hex);
            }
            return hexString.toString();
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e); //TODO: Custom Exception
        }
    }

    private static String generateRandomData() {
        SecureRandom random = new SecureRandom();
        byte[] bytes = new byte[32];
        random.nextBytes(bytes);
        return bytes.toString();
    }
}
