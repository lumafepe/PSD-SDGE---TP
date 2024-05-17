package org.client.crdts;

import org.client.crdts.base.Operation;
import org.client.crdts.records.Rating;
import org.messages.central.Classification;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

public class GOSet implements Serializable {

    private final HashSet<Rating> ratings;

    public GOSet() {
        this.ratings = new HashSet<>();
    }

    public HashSet<Rating> getRatings(){
        return this.ratings;
    }

    public boolean containsRating(Rating rating) {
        return this.ratings.contains(rating);
    }

    public boolean canRate(String user) {
        return this.ratings.stream().map(Rating::user).noneMatch(user::equals);
    }

    public Operation<Rating> addRating(String filename, String user, int rating) {
        Operation<Rating> operation = new Operation<>("rate", user, filename, rating);

        this.applyAddRatingOperation(operation);
        return operation;
    }

    public void applyAddRatingOperation(Operation<Rating> operation){
        for (Rating rating : ratings) {
            if (rating.fileName().equals(operation.fileName) && rating.user().equals(operation.user)) {
                return;
            }
        }
        this.ratings.add(new Rating(operation.user, operation.fileName, operation.rating));
    }

    public Set<Classification> getFileRatings() {
        return this.ratings.stream()
                .map(rat -> Classification.newBuilder().setValue(rat.rating()).setUsername(rat.user()).build())
                .collect(Collectors.toSet());
    }
}


