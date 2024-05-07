package org.dht;

import com.google.protobuf.ByteString;
import dht.messages.*;
import io.reactivex.rxjava3.core.BackpressureStrategy;
import io.reactivex.rxjava3.core.Flowable;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.dht.config.ConfigManager;

import java.io.*;
import java.net.InetSocketAddress;
import java.util.*;

public class DHTController {
    private static final Logger logger = LogManager.getLogger();
    private static final ConfigManager configManager = ConfigManager.getInstance();

    private MetadataManager metadataManager;


    public DHTController(MetadataManager manager) {
        metadataManager = manager;
    }

    public Flowable<WriteRequest> transfer(TransferRequest request) {
        logger.info("transfer request");
        return Flowable.create(sub -> {
            metadataManager.serverIsEntering(new InetSocketAddress(request.getIp(), request.getPort()), request.getTokenList());
            Queue<String> hashesToTransfer = hashesToTransfer(request.getTokenList());

            while(!hashesToTransfer.isEmpty()) {
                String cur = hashesToTransfer.peek();

                try (FileInputStream file = new FileInputStream(configManager.getConfig().getDht().getBaseDirectory() + cur)) {
                    byte[] buffer = new byte[4096];
                    int read = file.read(buffer);
                    long offset = 0;
                    while(read > 0) {
                        sub.onNext(WriteRequest.newBuilder()
                                    .setOffset(offset)
                                        .setHash(cur)
                                    .setData(ByteString.copyFrom(buffer, 0, read))
                                    .build());
                        offset += read;
                        read = file.read(buffer);
                    }

                } catch(IOException e) {
                    logger.error("An exception occurred: ", e);
                    sub.onComplete();
                    return;
                }

                hashesToTransfer.remove();
            }
            sub.onComplete();
        }, BackpressureStrategy.BUFFER);
    }

    public Flowable<ReadResponse> read(ReadRequest request) {
        if(!metadataManager.isReadAuthoritative(request.getHash())) {
            logger.info("ReadRequest Hash: {} - not authoritative", request.getHash());
            return Flowable.just(ReadResponse.newBuilder()
                    .setSuccess(Status.HASH_NOT_FOUND)
                    .setMessage("Not authoritative for hash " + request.getHash())
                    .build());
        }

        return Flowable.create(sub -> {
            try(FileInputStream stream = new FileInputStream(
                    configManager.getConfig().getDht().getBaseDirectory() + request.getHash())) {
                logger.info("ReadRequest Hash: {}", request.getHash());
                byte[] buffer = new byte[4096];
                while(stream.read(buffer) != -1) {
                    sub.onNext(ReadResponse.newBuilder()
                            .setSuccess(Status.SUCCESS)
                            .setData(ByteString.copyFrom(buffer))
                            .build());
                }
                sub.onComplete();
            } catch(FileNotFoundException e) {
                sub.onNext(ReadResponse.newBuilder()
                        .setSuccess(Status.HASH_NOT_FOUND)
                        .setMessage("Hash does not exist")
                        .build());
                sub.onComplete();
            } catch (IOException e) {
                logger.error("An exception occurred: ", e);
                sub.onNext(ReadResponse.newBuilder()
                        .setSuccess(Status.IO_ERROR)
                        .setMessage("Unable to complete I/O")
                        .build());
                sub.onComplete();
            }
        }, BackpressureStrategy.BUFFER);
    }

    public WriteResponse write(WriteRequest request) {
        if(!metadataManager.isWriteAuthoritative(request.getHash())) {
            logger.info("WriteRequest Hash: {} - not authoritative", request.getHash());
            return WriteResponse.newBuilder()
                    .setSuccess(Status.HASH_NOT_FOUND)
                    .setMessage("Not authoritative for hash " + request.getHash())
                    .build();
        }

        logger.info("WriteRequest Hash: {} Offset: {}", request.getHash(), request.getOffset());
        try (RandomAccessFile file = new RandomAccessFile(configManager.getConfig().getDht().getBaseDirectory() + request.getHash(), "rw")) {
            file.seek(request.getOffset());
            file.write(request.getData().toByteArray());

            return WriteResponse.newBuilder()
                    .setSuccess(Status.SUCCESS)
                    .build();
        } catch(IOException e) {
            logger.error("An exception occurred: ", e);
            return WriteResponse.newBuilder()
                    .setSuccess(Status.IO_ERROR)
                    .setMessage("I/O Error")
                    .build();
        }
    }

    public WriteResponse mergeWriteResponses(WriteResponse r1, WriteResponse r2) {
        if(r1.getSuccess() != Status.SUCCESS) {
            return r1;
        }
        return r2;
    }

    private Queue<String> hashesToTransfer(Collection<Long> tokens) {
        Set<String> result = new HashSet<>();

        File directory = new File(configManager.getConfig().getDht().getBaseDirectory());
        for(File file : directory.listFiles()) {
            String hash = file.getName();
            long position = TokenGenerator.hashToRing(hash, ConfigManager.getInstance().getConfig().getDht().getMod());

            for(Long token : tokens) {
                if(metadataManager.shouldBeTransferred(position, token)) {
                    result.add(hash);
                }
            }
        }

        return new ArrayDeque<>(result);
    }
}
