package org.dht;

import dht.messages.Rx3DHTServiceGrpc;
import dht.messages.TransferRequest;
import dht.messages.central.Message;
import dht.messages.central.NodeInfo;
import dht.messages.central.Type;
import io.grpc.ManagedChannelBuilder;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.dht.config.ConfigManager;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.util.*;

public class StartupHandler implements Runnable {
    private MetadataManager metadataManager;

    public StartupHandler(MetadataManager manager) {
        this.metadataManager = manager;
    }

    public void run() {
        Logger logger = LogManager.getLogger();
        logger.info("Announcing entry");
        List<NodeInfo> nodes = announceEntry();
        logger.info("Entry announced. Computing servers to contact");
        Map<Long, InetSocketAddress> serversToContact = computeServersToCopyFrom(nodes);
        logger.info("Transferring data");
        copyData(serversToContact);
        logger.info("Announcing entry terminated");
        concludeEntry();
        logger.info("Entry terminated");
    }

    private List<NodeInfo> announceEntry() {
        Message reply = send(Message.newBuilder()
                            .setType(Type.STARTENTRANCE)
                            .setNodeInfo(NodeInfo.newBuilder()
                                    .addAllTokens(metadataManager.getMyTokens())
                                    .setPort(ConfigManager.getInstance().getConfig().getDht().getPort())
                                    .setIp(ConfigManager.getInstance().getConfig().getDht().getAddress())
                                    .build())
                            .build());

        if(reply.getType() == Type.ERRORREPLY) {
            Logger logger = LogManager.getLogger();
            logger.error("Node already connecting. Retrying in 60 seconds");
            
            try {
                Thread.sleep(1000);
            } catch(InterruptedException e){}

            return announceEntry();
        }
        return reply.getNodesInfosList();
    }

    private Map<Long, InetSocketAddress> computeServersToCopyFrom(List<NodeInfo> nodeInfos) {
        Map<Long, InetSocketAddress> tokenServerMapping = new HashMap<>();
        TreeSet<Long> serverTokens = new TreeSet<>();
        processNodes(nodeInfos, tokenServerMapping, serverTokens);
        if (serverTokens.isEmpty()) return new HashMap<>();
        else return computeServersToCopyFrom(tokenServerMapping, serverTokens);
    }

    private Map<Long, InetSocketAddress> computeServersToCopyFrom(Map<Long, InetSocketAddress> tokenServerMapping,
                                                                  TreeSet<Long> serverTokens) {
        Map<Long, InetSocketAddress> result = new HashMap<>();

        for(Long token : metadataManager.getMyTokens()) {
            Long nextToken = serverTokens.ceiling(token);
            if(nextToken == null)
                nextToken = serverTokens.first();

            result.put(token, tokenServerMapping.get(nextToken));
        }

        return result;
    }

    private void processNodes(List<NodeInfo> nodeInfos,
                              Map<Long, InetSocketAddress> tokenServerMapping, TreeSet<Long> serverTokens) {

        for(NodeInfo node : nodeInfos) {

            if(node.getPort() == ConfigManager.getInstance().getConfig().getDht().getPort() && node.getIp().equals(ConfigManager.getInstance().getConfig().getDht().getAddress()) ){
                continue;
            }

            InetSocketAddress address = new InetSocketAddress(node.getIp(), node.getPort());
            List<Long> nodeTokens = new ArrayList<>();

            for(int i = 0; i < node.getTokensCount(); i++) {
                Long token = node.getTokens(i);
                nodeTokens.add(token);
                serverTokens.add(token);
                tokenServerMapping.put(token, address);
            }
            metadataManager.serverEntered(address, nodeTokens);
        }
    }

    private void copyData(Map<Long, InetSocketAddress> serversToContact) {
        Logger logger = LogManager.getLogger();
        for(InetSocketAddress server : new HashSet<>(serversToContact.values())) {
            logger.info("Transfering from " + server.toString());

            var channel = ManagedChannelBuilder.forAddress(server.getHostName(), server.getPort())
                    .usePlaintext()
                    .build();
            var stub = Rx3DHTServiceGrpc.newRxStub(channel);
            stub.transfer(Single.just(TransferRequest.newBuilder()
                            .addAllToken(metadataManager.getMyTokens())
                            .build())
                            .subscribeOn(Schedulers.io()))
                    .blockingSubscribe(req -> new DHTController(metadataManager).write(req));
        }
    }

    private void concludeEntry() {
        send(Message.newBuilder()
                .setType(Type.ENDENTRANCE)
                .setNodeInfo(NodeInfo.newBuilder()
                        .addAllTokens(metadataManager.getMyTokens())
                        .setPort(ConfigManager.getInstance().getConfig().getDht().getPort())
                        .setIp(ConfigManager.getInstance().getConfig().getDht().getAddress())
                        .build())
                .build());
    }

    private Message send(Message msg) {
        Logger logger = LogManager.getLogger();

        try (Socket clientSocket = new Socket(ConfigManager.getInstance().getConfig().getServer().getAddress() , ConfigManager.getInstance().getConfig().getServer().getPort())) {
            clientSocket.getOutputStream().write(msg.toByteArray());
            byte[] buffer = new byte[1048576];
            int read = clientSocket.getInputStream().read(buffer);
            Message response = Message.parseFrom(ByteBuffer.wrap(buffer, 0, read));
            return response;
        } catch(UnknownHostException e) {
            logger.fatal("An exception occurred when sending message to central server: ", e);
            System.exit(1);
        } catch(IOException e) {
            logger.fatal("An exception occurred when sending message to central server: ", e);
            System.exit(1);
        }
        return null;
    }
}
