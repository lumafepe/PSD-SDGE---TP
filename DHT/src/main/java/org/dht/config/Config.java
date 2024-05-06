package org.dht.config;

public class Config {

    private DHTConfig dht;
    private ServerConfig server;

    public DHTConfig getDht() {
        return dht;
    }
    public void setDht(DHTConfig dht) {
        this.dht = dht;
    }

    public ServerConfig getServer() {
        return server;
    }

    public void setServer(ServerConfig server) {
        this.server = server;
    }
}


