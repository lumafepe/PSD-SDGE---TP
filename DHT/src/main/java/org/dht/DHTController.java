package org.dht;

import com.google.protobuf.ByteString;
import dht.messages.*;
import io.reactivex.rxjava3.core.Flowable;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Iterator;

public class DHTController {
    private FileMap map;
    private static final Logger logger = LogManager.getLogger();

    public DHTController(FileMap map) {
        this.map = map;
    }

    public Flowable<ReadResponse> read(ReadRequest request) {
        return Flowable.fromIterable(doReads(request.getHash()));
    }

    public WriteResponse write(WriteRequest request) {
        logger.info("WriteRequest Hash: {} Offset_ {}", request.getHash(), request.getOffset());
        try {
            this.map.write(request.getHash(), request.getOffset(), request.getData().toByteArray());
            return WriteResponse.newBuilder()
                    .setSuccess(Status.SUCCESS)
                    .build();
        } catch(FileNotFoundException e) {
            logger.error("An exception occurred: ", e);
            return WriteResponse.newBuilder()
                    .setSuccess(Status.IO_ERROR)
                    .setMessage("Unable to create file")
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

    public Iterable<ReadResponse> doReads(String hash) {
        return new Iterable<ReadResponse>() {
            @NotNull
            @Override
            public Iterator<ReadResponse> iterator() {
                return new Iterator<ReadResponse>() {
                    long offset = 0;
                    boolean hasMore = false;
                    @Override
                    public boolean hasNext() {
                        return hasMore;
                    }

                    @Override
                    public ReadResponse next() {
                        //TODO: Configurable parameters
                        byte[] buffer = new byte[4096];
                        try {
                            int bytes = map.read(hash, offset, buffer);
                            hasMore = bytes == 4096;
                            return ReadResponse.newBuilder()
                                    .setSuccess(Status.SUCCESS)
                                    .setData(ByteString.copyFrom(buffer))
                                    .setMoreData(hasMore)
                                    .build();
                        } catch(FileNotFoundException e) {
                            hasMore = false;
                            logger.error("An exception occurred: ", e);
                            return ReadResponse.newBuilder()
                                    .setSuccess(Status.HASH_NOT_FOUND)
                                    .setMessage("Hash does not exist")
                                    .build();
                        } catch(IOException e) {
                            hasMore = false;
                            logger.error("An exception occurred: ", e);
                            return ReadResponse.newBuilder()
                                    .setSuccess(Status.IO_ERROR)
                                    .setMessage("I/O Error")
                                    .build();
                        }
                    }
                };
            }
        };
    }
}
