package org.dht;

import com.google.protobuf.ByteString;
import dht.messages.*;
import io.reactivex.rxjava3.core.BackpressureStrategy;
import io.reactivex.rxjava3.core.Flowable;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Iterator;
import java.util.concurrent.Flow;

public class DHTController {
    private static final Logger logger = LogManager.getLogger();
    private static final ConfigManager configManager = ConfigManager.getInstance();

    private MetadataManager metadataManager;


    public DHTController(MetadataManager manager) {
        metadataManager = manager;
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
                    configManager.getBaseDirectory() + request.getHash())) {
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
        try (RandomAccessFile file = new RandomAccessFile(configManager.getBaseDirectory() + request.getHash(), "rw")) {
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
}