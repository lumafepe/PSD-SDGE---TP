package org.client;

import client.p2p.OperationMessage;
import org.client.crdts.base.Operation;
import org.client.utils.VectorClock;
import org.zeromq.ZMQ;

import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

public class Broadcaster {

    private List<String> network;
    private ZMQ.Socket router;

    private final VectorClock version;
    private final int self;

    private final Queue<Operation> pending;

    public Broadcaster(List<String> network, int self, int selfValue) {
        this.network = network;
        this.self = self;

        this.version = new VectorClock(network.size(), self);
        for (int i = 0; i < selfValue; i++) {
            this.version.increment(self);
        }

        this.pending = new LinkedList<>();
    }

    private boolean canDeliver(Operation op) {
        return false;
    }

    private void loopPending() {

        for (Operation op : this.pending) {
            if (this.canDeliver(op)) {
                this.pending.remove(op);

                // todo: do stuff with op
                this.version.increment(0); // todo: get index from message
            }
        }
    }

    private void deliver(Operation op) {

        this.version.increment(0); // todo: get index from message
        // todo: do stuff with op
        this.loopPending();
    }

    public void broadcast(Operation op) {
        this.version.increment(this.self);

        OperationMessage message = op.getOperationMessage();
        for (String identity : this.network) {
            router.sendMore(identity);
            router.sendMore("");
            router.send(message.toByteArray(), 0);
        }
    }

    public void receive(Operation op) {
        if (!canDeliver(op)) {
            this.pending.add(op);
            return;
        }
        this.deliver(op);
    }
}
