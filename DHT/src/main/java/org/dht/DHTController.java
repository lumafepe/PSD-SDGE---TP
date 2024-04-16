package org.dht;

import com.google.protobuf.ByteString;
import dht.messages.*;
import io.reactivex.rxjava3.core.BackpressureStrategy;
import io.reactivex.rxjava3.core.Flowable;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.dht.config.Manager;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;

public class DHTController {
    private static final Logger logger = LogManager.getLogger();
    private static final Manager configManager = Manager.getInstance();


    public DHTController() {}

    public Flowable<ReadResponse> read(ReadRequest request) {
        return Flowable.create(sub -> {
            try(FileInputStream stream = new FileInputStream(
                    configManager.getDHT().getBaseDirectory() + request.getHash())) {
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
                sub.onNext(ReadResponse.newBuilder()
                        .setSuccess(Status.IO_ERROR)
                        .setMessage("Unable to complete I/O")
                        .build());
                sub.onComplete();
            }
        }, BackpressureStrategy.BUFFER);
    }

    public WriteResponse write(WriteRequest request) {
        logger.info("WriteRequest Hash: {} Offset_ {}", request.getHash(), request.getOffset());
        try (RandomAccessFile file = new RandomAccessFile(configManager.getDHT().getBaseDirectory() + request.getHash(), "rw")) {
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
