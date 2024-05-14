package org.client.crdts;

import org.client.crdts.base.Operation;
import org.client.crdts.records.Rating;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

public class GCounter implements Serializable {

    private final Map<String, Integer> values;

    public GCounter() {
        this.values = new HashMap<String, Integer>();
    }

    public GCounter(HashMap<String, Integer> values) {
        this.values = values;
    }

    public int getValue(String filename) {
        return this.values.get(filename);
    }

    public void increment(String filename) {
        if (this.values.containsKey(filename)) {
            this.values.put(filename, this.values.get(filename) + 1);
        }
        this.values.put(filename, 1);
    }

    public void applyIncrementOperation(Operation<Rating> op) {

        String filename = op.element.fileName();
        int rating = op.element.rating();

        this.values.put(filename, this.values.get(filename) + rating);
    }
}
