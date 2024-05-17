package org.client.controllers;

import com.google.protobuf.ByteString;
import dht.messages.*;
import io.grpc.ManagedChannelBuilder;
import io.reactivex.rxjava3.core.BackpressureStrategy;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;
import org.client.utils.Hasher;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class DHTController {

    private final String SERVER_ADDRESS;
    private final int SERVER_PORT;

    public DHTController(String serverAddress, int serverPort) {
        this.SERVER_ADDRESS = serverAddress;
        this.SERVER_PORT = serverPort;
    }

    @Contract(value = "_, _ -> new", pure = true)
    public static @NotNull DHTController at(String serverAddress, int serverPort) {
        return new DHTController(serverAddress, serverPort);
    }

    public Flowable<WriteRequest> readFile(String path) {
        String hash = Hasher.digest(path);

        return Flowable.create(sub -> {
            try(FileInputStream stream = new FileInputStream(path)) {
                long offset = 0;
                int bytesRead;
                byte[] buffer = new byte[4096];
                while((bytesRead = stream.read(buffer)) != -1) {
                    sub.onNext(WriteRequest.newBuilder()
                            .setHash(hash)
                            .setData(ByteString.copyFrom(buffer.clone()))
                            .setOffset(offset)
                            .build());
                    offset += bytesRead;
                }
            } catch(FileNotFoundException e) {
                //Log if needed
            } catch (IOException e) {
                //Log if needed
            }
            sub.onComplete();
        }, BackpressureStrategy.BUFFER);
    }


    public Status setFile(String filepath) throws IOException {

        var channel = ManagedChannelBuilder
                .forAddress(this.SERVER_ADDRESS, this.SERVER_PORT)
                .usePlaintext()
                .build();

        var stub = Rx3DHTServiceGrpc.newRxStub(channel);

        Status status = stub.write(readFile(filepath))
                .map(WriteResponse::getSuccess)
                .blockingGet();

        channel.shutdown();
        return status;
    }

    public Status getFile(String hash, String output) throws IOException {

        Status status;

        var channel = ManagedChannelBuilder
                .forAddress(this.SERVER_ADDRESS, this.SERVER_PORT)
                .usePlaintext()
                .build();

        var stub = Rx3DHTServiceGrpc.newRxStub(channel);
        try (FileOutputStream file = new FileOutputStream(output)) {

            ReadRequest req = ReadRequest.newBuilder().setHash(hash).build();

            status = stub.read(Single.just(req))
                    .observeOn(Schedulers.io())
                    .map(m -> {
                        file.write(m.getData().toByteArray());
                        return m.getSuccess();
                    })
                    .reduce((r1, r2) -> (r1 != Status.SUCCESS) ? r1 : r2)
                    .blockingGet();
        }

        channel.shutdown();
        return status;
    }

}
