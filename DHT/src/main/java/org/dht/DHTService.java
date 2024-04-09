package org.dht;

import dht.messages.*;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.core.Flowable;

public class DHTService extends Rx3DHTServiceGrpc.DHTServiceImplBase {
    public Flowable<ReadResponse> read(Single<ReadRequest> request) {
        return null;
    }

    public Single<WriteResponse> write(Flowable<WriteRequest> request) {
        return null;
    }
}
