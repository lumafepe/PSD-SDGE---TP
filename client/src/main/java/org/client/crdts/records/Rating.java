package org.client.crdts.records;

import java.io.Serializable;

public record Rating(String user, String fileName, int rating) implements Serializable {
    @Override
    public String toString() {
        return "USER: " + user + ", FILE: " + fileName + ", RATING: " + rating;
    }
}