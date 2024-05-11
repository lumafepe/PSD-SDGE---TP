package org.client.network;

import org.client.crdts.Album;
import org.client.crdts.base.Operation;
import org.client.messages.BroadcastMessage;
import org.client.messages.ClientMessage;
import org.client.utils.VectorClock;

import java.io.IOException;
import java.util.*;

public class Broadcaster {

    private final Network network;

    private VectorClock version;
    private int self;

    private Queue<BroadcastMessage> pending;
    private final Album crdts = Album.getInstance();

    private Map<String, VectorClock> joinTimestamps = new HashMap<>();
    private Map<String, List<Boolean>> receivedJoin = new HashMap<>();

    private ArrayList<VectorClock> waiting = new ArrayList<>();

    public Broadcaster(Network network, int self, int selfValue) {
        this.network = network;
        this.self = self;

        this.version = new VectorClock(network.size() + 1, self);
        for (int i = 0; i < selfValue; i++) {
            this.version.increment(self);
        }

        this.pending = new LinkedList<>();
    }

    public void initialize(int self, int selfValue){
        this.self = self;
        this.version = new VectorClock(network.size() + 1, self);
        for (int i = 0; i < selfValue; i++) {
            this.version.increment(self);
        }
        this.pending = new LinkedList<>();
    }

    public void addToVector(int position, int clock){
        VectorClock vectorClock = new VectorClock(position+1, position);
        vectorClock.setClockPosition(position, clock);
        this.version.merge(vectorClock);
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
        for(VectorClock vc : this.waiting){
            if (vc.getClock().get(vc.getId()) == this.version.getClock().get(vc.getId())){
                this.waiting.remove(vc);
            }
        }
        crdts.handleOperation(message.operation());
        this.loopPending();
    }

    public void broadcast(Operation op) throws IOException {

        BroadcastMessage message = new BroadcastMessage(this.self, this.version, op);
        ClientMessage clientMessage = new ClientMessage("op", message, null, null, -1, -1, null, null);
        this.version.increment(this.self);
        this.network.loopSend(clientMessage.asBytes()); // loop over peer network and send message

    }

    public void receive(BroadcastMessage message) {
        if (!canDeliver(message)) {
            this.pending.add(message);
            return;
        }
        this.deliver(message);
    }

    public void setVersion(VectorClock vc, int clock, int position){
        VectorClock vectorClock = new VectorClock(position+1, position);
        vectorClock.setClockPosition(position, clock);
        this.version = vc;
        this.version.merge(vectorClock);
    }

    public VectorClock getVersion() {
        return this.version;
    }

    public void setPending(Queue<BroadcastMessage> pending){
        this.pending = pending;
    }

    public Queue<BroadcastMessage> getPending(){
        return this.pending;
    }

    public void addForwardingNode(String forwardingNode){
        this.joinTimestamps.put(forwardingNode, this.version);
        List<Boolean> list = new ArrayList<>();
        for (int i=0; i<this.version.getLength(); i++)
            list.add(false);
        this.receivedJoin.put(forwardingNode, list);
    }

    public void forward(ClientMessage message) throws IOException {
        VectorClock msgVC = message.message().version();
        for (String forwardingNode : this.joinTimestamps.keySet()) {
            // Check if node that sent the message knows about this joining node

            boolean received = this.receivedJoin.get(forwardingNode).get(msgVC.getId());
            VectorClock joinTimestamp = this.joinTimestamps.get(forwardingNode);
            if (!received){
                if (msgVC.happensBefore(joinTimestamp)){
                    ClientMessage forwardMessage = new ClientMessage("forward", message.message(), null, null, -1, -1, null, null);
                    this.network.send(forwardMessage.asBytes(), forwardingNode);
                } else {
                    this.receivedJoin.get(forwardingNode).add(msgVC.getId(), true);
                    if (this.receivedJoin.get(forwardingNode).stream().reduce(true, (aBoolean, aBoolean2) -> aBoolean && aBoolean2)){
                        this.receivedJoin.remove(forwardingNode);
                        this.joinTimestamps.remove(forwardingNode);
                    }
                }
            }
        }
    }

    public void addWaitingMsg(VectorClock vc){
        if (this.version.getClock().get(vc.getId()) > vc.getClock().get(vc.getId())){
            this.waiting.add(vc);
        }
    }

    public boolean canLeave(){
        return this.waiting.isEmpty();
    }
}
