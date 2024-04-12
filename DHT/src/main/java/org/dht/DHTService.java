package org.dht;

import com.google.protobuf.Message;
import dht.messages.*;
import io.grpc.stub.StreamObserver;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.core.Flowable;

public class DHTService extends Rx3DHTServiceGrpc.DHTServiceImplBase {
    private DHTController controller;

    public DHTService(FileMap map) {
        this.controller = new DHTController(map);
    }

    public Flowable<ReadResponse> read(Single<ReadRequest> request) {
        return request.toFlowable()
                .flatMap(req -> controller.read(req));
    }

    public Single<WriteResponse> write(Flowable<WriteRequest> requests) {
        return requests.map(controller::write)
                .reduce(controller::mergeWriteResponses)
                .toSingle();
    }

    public Single<dht.messages.Message> echoMsg(Single<dht.messages.Message> msg) {
        return msg;
    }


}
