package org.example;

import java.io.Serializable;

public class Rating implements Serializable {
    public String user;
    public String fileName;
    public int rating;

    public Rating(String user, String fileName, int rating) {
        this.user = user;
        this.fileName = fileName;
        this.rating = rating;
    }

    @Override
    public String toString() {
        return "USER: " + user + ", FILE: " + fileName + ", RATING: " + rating;
    }
}