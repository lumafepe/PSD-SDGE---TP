package org.dht;

import com.google.common.io.ByteStreams;
import dht.messages.central.Message;
import dht.messages.central.NodeInfo;
import dht.messages.central.Type;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.w3c.dom.Node;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.UnknownHostException;
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
        logger.info("Transfering data");
        copyData(serversToContact);
        logger.info("Announcing entry terminated");
        concludeEntry();
        logger.info("Entry terminated");
    }

    private List<NodeInfo> announceEntry() {
        Message reply = send(Message.newBuilder()
                            .setType(Type.STARTENTRANCE)
                            .setNodeInfo(NodeInfo.newBuilder()
                                    .addAllTokens(metadataManager.getMyTokens()))
                            .build());

        return reply.getNodesInfoList();
    }

    private Map<Long, InetSocketAddress> computeServersToCopyFrom(List<NodeInfo> nodeInfos) {
        Map<Long, InetSocketAddress> tokenServerMapping = new HashMap<>();
        TreeSet<Long> serverTokens = new TreeSet<>();

        processNodes(nodeInfos, tokenServerMapping, serverTokens);
        return computeServersToCopyFrom(tokenServerMapping, serverTokens);
    }

    private Map<Long, InetSocketAddress> computeServersToCopyFrom(Map<Long, InetSocketAddress> tokenServerMapping,
                                                                  TreeSet<Long> serverTokens) {
        Map<Long, InetSocketAddress> result = new HashMap<>();

        for(String tokenStr : metadataManager.getMyTokens()) {
            Long token = Long.parseLong(tokenStr);

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
            InetSocketAddress address = new InetSocketAddress(node.getIp(), Integer.parseInt(node.getPort()));
            List<Long> nodeTokens = new ArrayList<>();

            for(int i = 0; i < node.getTokensCount(); i++) {
                Long token = Long.parseLong(node.getTokens(i));
                nodeTokens.add(token);
                serverTokens.add(token);
                tokenServerMapping.put(token, address);
            }
            metadataManager.serverEntered(address, nodeTokens);
        }
    }

    private void copyData(Map<Long, InetSocketAddress> serversToContact) {
        Logger logger = LogManager.getLogger();
        for(Map.Entry<Long, InetSocketAddress> entry : serversToContact.entrySet()) {
            logger.info("Transfering from ", entry.getValue().toString());
        }
    }

    private void concludeEntry() {
        send(Message.newBuilder()
                .setType(Type.ENDENTRANCE)
                .setNodeInfo(NodeInfo.newBuilder()
                        .addAllTokens(metadataManager.getMyTokens()))
                .build());
    }

    private Message send(Message msg) {
        Logger logger = LogManager.getLogger();
        //TODO: Change
        try(Socket clientSocket = new Socket("127.0.0.1", 4321)) {
            clientSocket.getOutputStream().write(msg.toByteArray());
            Message response = Message.parseFrom(clientSocket.getInputStream());
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
