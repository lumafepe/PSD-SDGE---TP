package org.client.crdts;

import org.client.crdts.base.Operation;
import org.client.crdts.records.Rating;

import java.io.Serializable;
import java.util.HashSet;

public class GOSet implements Serializable {
    private final HashSet<Rating> ratings;

    public GOSet() {
        ratings = new HashSet<>();
    }

    public HashSet<Rating> getRatings(){
        return ratings;
    }

    public boolean containsRating(Rating rating){
        return ratings.contains(rating);
    }

    public Operation addRating(String fileName, String user, int rating) {
        Operation ratingOperation = new Operation("rate", user, fileName, rating);
        applyAddRatingOperation(ratingOperation);
        return ratingOperation;
    }

    public void applyAddRatingOperation(Operation operation){
        for (Rating rating : ratings) {
            if (rating.fileName().equals(operation.fileName) && rating.user().equals(operation.user)) {
                return;
            }
        }
        this.ratings.add(new Rating(operation.user, operation.fileName, operation.rating));
    }
}


