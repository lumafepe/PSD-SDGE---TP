package org.dht.config;

import com.google.common.net.InetAddresses;
import org.dht.exceptions.InvalidConfigurationFileException;

public class ServerConfig {

    private String address;
    private int port;

    public int getPort() {
        return port;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public void setPort(int port) {
        this.port = port;
    }

    public void validate() throws InvalidConfigurationFileException {

        if (this.port <= 0 || this.port >= 65535) {
            throw new InvalidConfigurationFileException("server.port '" + this.port + "' value must be between 0 and 65535");
        }

        if (!InetAddresses.isInetAddress(this.address)) {
            throw new InvalidConfigurationFileException("server.address '" + this.address + "' is not a valid ipv4 address");
        }
    }
}
