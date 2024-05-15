package org.client.crdts;

import org.client.crdts.base.Operation;
import org.client.crdts.records.Rating;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

public class GCounter implements Serializable {

    private final Map<String, Integer> values;

    public GCounter() {
        this.values = new HashMap<>();
    }

    public int getValue(String filename) {
        return this.values.get(filename);
    }

    public void increment(String filename, int value) {
        if (this.values.containsKey(filename)) {
            this.values.put(filename, this.values.get(filename) + value);
        }
        else this.values.put(filename, value);
    }

    public void applyIncrementOperation(Rating r) {

        String filename = r.fileName();
        int rating = r.rating();

        this.values.put(filename, this.values.get(filename) + rating);
    }
}
