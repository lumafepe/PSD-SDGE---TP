package org.client.crdts;

import org.messages.central.File;

import java.io.Serializable;

public class CRDTS implements Serializable {
    public GOSet fileRatingsCRDT = null;
    public ORset<File> filesCRDT = null;
    public ORset<String> usersCRDT = null;
}
