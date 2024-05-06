package org.client;

import org.client.crdts.Album;
import org.client.crdts.base.Operation;
import org.client.utils.VectorClock;

import java.io.IOException;
import java.util.LinkedList;
import java.util.Queue;

public class Broadcaster {

    private final Network network;

    private final VectorClock version;
    private final int self;

    private final Queue<BroadcastMessage> pending;
    private final Album crdts = Album.getInstance();

    public Broadcaster(Network network, int self, int selfValue) {
        this.network = network;
        this.self = self;

        this.version = new VectorClock(network.size() + 1, self);
        for (int i = 0; i < selfValue; i++) {
            this.version.increment(self);
        }

        this.pending = new LinkedList<>();
    }

    private boolean canDeliver(BroadcastMessage message) {
        return this.version.happensBefore(message.version());
    }

    private void loopPending() {

        for (BroadcastMessage m : this.pending) {
            if (this.canDeliver(m)) {
                this.pending.remove(m); // todo: implement equals in VectorClock & BroadcastMessage ?
                crdts.handleOperation(m.operation());
                this.version.increment(m.index());
            }
        }
    }
    private void deliver(BroadcastMessage message) {
        this.version.increment(message.index());
        crdts.handleOperation(message.operation());
        this.loopPending();
    }

    public void broadcast(Operation op) throws IOException {

        BroadcastMessage message = new BroadcastMessage(this.self, this.version, op);
        this.version.increment(this.self);
        this.network.loopSend(message.asBytes()); // loop over peer network and send message
    }

    public void receive(BroadcastMessage message) {
        if (!canDeliver(message)) {
            this.pending.add(message);
            return;
        }
        this.deliver(message);
    }
}
