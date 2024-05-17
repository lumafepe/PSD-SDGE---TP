package org.dht;

import com.google.protobuf.Message;
import dht.messages.*;
import io.grpc.stub.StreamObserver;
import io.reactivex.rxjava3.core.Observable;
import io.reactivex.rxjava3.core.Scheduler;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class DHTService extends Rx3DHTServiceGrpc.DHTServiceImplBase {
    private DHTController controller;

    public DHTService(MetadataManager manager) {
        this.controller = new DHTController(manager);
    }

    public Flowable<ReadResponse> read(Single<ReadRequest> request) {
        return request.toFlowable().flatMap(this.controller::read)
                .subscribeOn(Schedulers.io());
    }

    public Single<WriteResponse> write(Flowable<WriteRequest> requests) {
        return requests
                .concatMap(req -> Flowable.just(this.controller.write(req)).subscribeOn(Schedulers.io()))
                .observeOn(Schedulers.computation())
                .reduce(this.controller::mergeWriteResponses)
                .toSingle();
    }

    public Flowable<WriteRequest> transfer(Single<TransferRequest> request) {
        return request
                .toFlowable()
                .flatMap(this.controller::transfer)
                .subscribeOn(Schedulers.io());
    }

    public Single<dht.messages.Message> echoMsg(Single<dht.messages.Message> msg) {
        return msg;
    }

}
