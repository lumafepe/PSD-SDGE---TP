package org.example;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CBcast extends Thread{
    private Map<String, Integer> versionVector = new HashMap<>(); // Map from node id to timestamp, assume node id as the client name
    private List<Message> pending = new ArrayList<>(); // List of messages to be delivered

    public void broadcast(Message msg){
        versionVector.put(msg.src, versionVector.get(msg.src) + 1);

        // Send message to socket
    }

    @Override
    public void run() {
        // Initialize sockets
        while (true){
            // Receive messages
        }
    }


}
