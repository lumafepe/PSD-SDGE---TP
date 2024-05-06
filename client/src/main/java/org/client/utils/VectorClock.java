package org.client.utils;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class VectorClock implements Serializable {

    public static void main(String[] args) {

        Random random = new Random();

        VectorClock v = new VectorClock(10, 2);
        for (int i = 0; i < 10; i++) {
            v.increment(random.nextInt(10));
        }

        VectorClock v2 = new VectorClock(10, 0);
    }

    private final List<Integer> clock;
    private final int id;

    public VectorClock(int vectorSize, int id) {
        this.id = id;
        this.clock = new ArrayList<>();

        for (int i = 0; i < vectorSize; i++) {
            this.clock.add(0);
        }
    }

    public void merge(VectorClock other) {

        if (other.getLength() <= this.getLength()) {
            return;
        }

        List<Integer> otherClock = other.getClock();
        for (int i = this.getLength(); i < other.getLength(); i++) {
            this.clock.add(otherClock.get(i));
        }
    }

    // todo: is the synchronized needed ?
    public synchronized void increment(int index) {
        this.clock.set(index, this.clock.get(index) + 1);
    }

    public boolean happensBefore(VectorClock other) {

        int id = other.getId();
        List<Integer> otherClock = other.getClock();

        if (other.getLength() > this.getLength()) {
            this.merge(other);
        }

        if (this.clock.get(id) != (otherClock.get(id) - 1)) {
            return false;
        }

        for (int i = 0; i < this.getLength(); i++) {
            if ((i != id) && (this.clock.get(i) < otherClock.get(i))) {
                return false;
            }
        }

        return true;
    }

    public int getLength() {
        return this.clock.size();
    }

    public List<Integer> getClock() {
        return this.clock; // this value comes from a message, shallow copy is fine
    }

    public int getId() {
        return id;
    }

    public String toString() {
        StringBuilder s = new StringBuilder();
        boolean flag = false;

        s.append("clock[");
        for (int j : clock) {
            if (flag)
                s.append(",");
            else
                flag = true;
            s.append(j);
        }
        s.append("]");

        return s.toString();
    }
}
