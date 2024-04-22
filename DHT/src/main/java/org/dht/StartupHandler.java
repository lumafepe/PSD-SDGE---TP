package org.dht;

import com.google.common.io.ByteStreams;
import com.google.protobuf.ByteString;
import dht.messages.Rx3DHTServiceGrpc;
import dht.messages.TransferRequest;
import dht.messages.WriteResponse;
import dht.messages.central.Message;
import dht.messages.central.NodeInfo;
import dht.messages.central.Type;
import io.grpc.ManagedChannelBuilder;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.w3c.dom.Node;

import java.io.IOException;
import java.io.OutputStream;
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
        for(Map.Entry<Long, InetSocketAddress> entry : serversToContact.entrySet()) {
            logger.info("Transfering from " + entry.getValue().toString());

            var channel = ManagedChannelBuilder.forAddress(entry.getValue().getHostName(), entry.getValue().getPort())
                    .usePlaintext()
                    .build();

            var stub = Rx3DHTServiceGrpc.newRxStub(channel);

            stub.transfer(Single.just(TransferRequest.newBuilder()
                            .addAllToken(metadataManager.getMyTokens())
                            .build())
                            .subscribeOn(Schedulers.io()))
                    .subscribe(req -> new DHTController(metadataManager).write(req));
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
