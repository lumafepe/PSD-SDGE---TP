package org.dht;

import com.google.protobuf.Message;
import dht.messages.*;
import io.grpc.stub.StreamObserver;
import io.reactivex.rxjava3.core.Scheduler;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class DHTService extends Rx3DHTServiceGrpc.DHTServiceImplBase {
    private DHTController controller;

    public DHTService() {
        this.controller = new DHTController();
    }

    public Flowable<ReadResponse> read(Single<ReadRequest> request) {
        return request.toFlowable().flatMap(this.controller::read)
                .subscribeOn(Schedulers.io());
    }

    public Single<WriteResponse> write(Flowable<WriteRequest> requests) {
        return requests
                .observeOn(Schedulers.io())
                .map(this.controller::write)
                .reduce(this.controller::mergeWriteResponses)
                .toSingle();
    }

    public Single<dht.messages.Message> echoMsg(Single<dht.messages.Message> msg) {
        return msg;
    }

}
