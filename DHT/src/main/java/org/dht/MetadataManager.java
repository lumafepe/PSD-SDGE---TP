package org.dht;

import org.apache.logging.log4j.LogManager;
import org.dht.config.Manager;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.util.*;

public class MetadataManager {
    private TreeSet<Long> myTokens;
    private TreeSet<Long> allReadTokens;

    private TreeSet<Long> allWriteTokens;

    private Map<Long, InetSocketAddress> ipMap;

    public MetadataManager() {
        generateTokens();

        this.allReadTokens = new TreeSet<>();
        this.allWriteTokens = new TreeSet<>();

        this.ipMap = new HashMap<>();

        InetAddress ip = null;
        try {
            ip  = InetAddress.getByName("0.0.0.0");
        } catch(UnknownHostException e) {
            LogManager.getLogger().warn("Unknown host 0.0.0.0");
        }


        for(Long l : this.myTokens) {
            this.allReadTokens.add(l);
            this.allWriteTokens.add(l);
            this.ipMap.put(l, new InetSocketAddress(ip, Manager.getInstance().getDHT().getPort()));
        }
    }

    public Set<Long> getMyTokens() {
        Set<Long> result = new TreeSet<>();
        result.addAll(this.myTokens);
        return result;
    }

    public boolean shouldBeTransferred(long filePosition, long serverPosition) {
        long responsibleToken = this.getResponsibleToken(filePosition);

        if(filePosition <= serverPosition && serverPosition <= responsibleToken)
            return true;

        if(responsibleToken == this.myTokens.first().longValue()
                && ((this.myTokens.ceiling(serverPosition) == null && filePosition <= serverPosition)) ||
                (serverPosition < this.myTokens.first().longValue() && filePosition >= responsibleToken))
            return true;

        return false;
    }

    public void serverEntered(InetSocketAddress address, Collection<Long> tokens) {
        this.allWriteTokens.addAll(tokens);
        this.allReadTokens.addAll(tokens);

        for(Long l : tokens) {
            this.ipMap.put(l, address);
        }
    }

    public void serverIsEntering(InetSocketAddress address, Collection<Long> tokens) {
        this.allWriteTokens.addAll(tokens);
    }

    public boolean isWriteAuthoritative(String hash) {
        return isAuthoritative(hash, allWriteTokens);
    }

    public boolean isReadAuthoritative(String hash) {
        return isAuthoritative(hash, allReadTokens);
    }

    private boolean isAuthoritative(String hash, TreeSet<Long> tokens) {
        long position = TokenGenerator.hashToRing(hash, Manager.getInstance().getDHT().getMod());

        Long token = tokens.ceiling(position);
        if(token == null) {
            token = tokens.first();
        }

        return myTokens.contains(token);
    }

    private void generateTokens() {
        Manager configManager = Manager.getInstance();
        this.myTokens = new TreeSet<>();
        this.myTokens.addAll(TokenGenerator.generateTokens(configManager.getDHT().getTokenCount(), configManager.getDHT().getMod()));
    }

    private long getResponsibleToken(long position) {
        Long responsibleToken = this.myTokens.ceiling(position);
        if(responsibleToken == null)
            responsibleToken = this.myTokens.first();

        return responsibleToken.longValue();
    }
}
