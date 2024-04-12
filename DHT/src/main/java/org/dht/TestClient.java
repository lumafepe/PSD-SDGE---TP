package org.dht;

import com.google.protobuf.ByteString;
import dht.messages.Message;
import dht.messages.Rx3DHTServiceGrpc;
import dht.messages.WriteRequest;
import dht.messages.WriteResponse;
import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.stub.StreamObserver;
import io.reactivex.rxjava3.core.BackpressureStrategy;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.Single;

import java.io.FileInputStream;
import java.io.IOException;
import java.security.SecureRandom;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;

public class TestClient {

    private static final String SERVER_ADDRESS = "localhost";
    private static final int SERVER_PORT = 4200;


    public static Iterable<byte[]> readFile(String file, int chunkSize) {
        List<byte[]> result = new ArrayList<>();
        try (FileInputStream inputStream = new FileInputStream(file)) {
            byte[] buffer = new byte[chunkSize]; // 4KB buffer

            int bytesRead;
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                // Process the chunk of data here
                result.add(buffer.clone());
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return result;
    }

    public static void main(String[] args) {
        var channel = ManagedChannelBuilder.forAddress("localhost", 4200)
                .usePlaintext()
                .build();
        var stub = Rx3DHTServiceGrpc.newRxStub(channel);
        stub.write(Flowable.fromIterable(readFile("/home/ruioliveira02/Pictures/background.jpg", 4096))
                        .map(n -> WriteRequest.newBuilder().setData(ByteString.copyFrom(n)).setHash("fsdafasdf").build())
                )
                .map(m -> m.getSuccessValue())
                .map(n -> "Result: " + n)
                .blockingSubscribe(System.out::println);
        /*stub.echoMsg(Single.just("Hello")
                        .map(n -> Message.newBuilder().setData(n).build())
                )
                .map(m -> m.getData())
                .map(n -> "Result: " + n)
                .blockingSubscribe(System.out::println);*/
    }

}
