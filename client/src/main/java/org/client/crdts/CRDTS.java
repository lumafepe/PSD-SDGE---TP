package org.client.crdts;

import org.messages.central.File;

import java.io.Serializable;

public class CRDTS implements Serializable {

    public GCounter fileRatingsCRDT = null;
    public GOSet fileVotersCRDT = null;

    public ORset<File> filesCRDT = null; // todo: does not need init ?
    public ORset<String> usersCRDT = null;
}
