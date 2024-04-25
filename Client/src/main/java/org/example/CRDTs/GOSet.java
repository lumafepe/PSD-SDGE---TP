package org.example.CRDTs;

import org.example.Rating;

import java.util.HashSet;

public class GOSet {
    private HashSet<Rating> ratings;

    public GOSet() {
        ratings = new HashSet<>();
    }

    public HashSet<Rating> getRatings(){
        return ratings;
    }

    public boolean containsRating(Rating rating){
        return ratings.contains(rating);
    }

    public Operation addRating(Rating rating) {
        this.ratings.add(rating);

        Operation ratingOperation = new Operation("add", rating, null, null);
        return ratingOperation;
    }
}


