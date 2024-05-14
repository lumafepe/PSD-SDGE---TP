package org.client.controllers;

import com.google.protobuf.ByteString;
import org.messages.dht.*;
import io.grpc.ManagedChannelBuilder;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;
import org.client.utils.Hasher;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.io.FileInputStream;
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

    private static @NotNull Iterable<WriteRequest> readFile(String filepath) throws IOException {

        String fileHash = Hasher.digest(filepath);
        List<WriteRequest> writeRequests = new ArrayList<>();

        try (FileInputStream inputStream = new FileInputStream(filepath)) {

            byte[] buffer = new byte[Hasher.BLOCK_SIZE];

            int bytesRead;
            long offset = 0;

            while ((bytesRead = inputStream.read(buffer)) != -1) {
                WriteRequest req = WriteRequest
                        .newBuilder()
                        .setHash(fileHash)
                        .setData(ByteString.copyFrom(buffer.clone()))
                        .setOffset(offset)
                        .build();

                writeRequests.add(req);
                offset += bytesRead;
            }
        }

        return writeRequests;
    }

    public Status setFile(String filepath) throws IOException {

        var channel = ManagedChannelBuilder
                .forAddress(this.SERVER_ADDRESS, this.SERVER_PORT)
                .usePlaintext()
                .build();

        var stub = Rx3DHTServiceGrpc.newRxStub(channel);

        Status status = stub.write(Flowable.fromIterable(readFile(filepath)))
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
                    .blockingFirst();
        }

        channel.shutdown();
        return status;
    }

}
