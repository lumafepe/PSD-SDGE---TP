package org.dht;

import com.google.protobuf.ByteString;
import dht.messages.*;
import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.stub.StreamObserver;
import io.reactivex.rxjava3.core.BackpressureStrategy;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.Single;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.security.SecureRandom;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;

public class TestClient {

    private static final String SERVER_ADDRESS = "localhost";
    private static final int SERVER_PORT = 4200;


    public static Iterable<WriteRequest> readFile(String file, int chunkSize) {
        List<WriteRequest> result = new ArrayList<>();
        try (FileInputStream inputStream = new FileInputStream(file)) {
            byte[] buffer = new byte[chunkSize]; // 4KB buffer

            int bytesRead;
            long offset = 0;
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                // Process the chunk of data here

                result.add(WriteRequest.newBuilder()
                        .setHash("asdasdd")
                        .setData(ByteString.copyFrom(buffer.clone()))
                        .setOffset(offset)
                        .build());
                offset += bytesRead;
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return result;
    }

    public static void main(String[] args) throws IOException {
        var channel = ManagedChannelBuilder.forAddress("localhost", 4200)
                .usePlaintext()
                .build();
        var stub = Rx3DHTServiceGrpc.newRxStub(channel);
        stub.write(Flowable.fromIterable(readFile("/home/ruioliveira02/Pictures/background.jpg", 4096))
                )
                .map(m -> m.getSuccessValue())
                .map(n -> "Result: " + n)
                .blockingSubscribe(System.out::println);

        FileOutputStream file = new FileOutputStream("/home/ruioliveira02/Pictures/teste213.jpg");
        stub.read(Single.just(ReadRequest.newBuilder().setHash("asdasdd").build())
                )
                .map(m -> {
                    file.write(m.getData().toByteArray());
                    return m.getSuccess();
                })
                .blockingSubscribe(System.out::println);
        file.close();

        /*stub.echoMsg(Single.just("Hello")
                        .map(n -> Message.newBuilder().setData(n).build())
                )
                .map(m -> m.getData())
                .map(n -> "Result: " + n)
                .blockingSubscribe(System.out::println);*/
    }

}
