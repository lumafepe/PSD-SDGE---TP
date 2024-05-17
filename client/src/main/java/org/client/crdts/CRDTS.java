package org.client.crdts;

import org.client.crdts.records.FileRecord;
import java.io.Serializable;

public class CRDTS implements Serializable {

    public GCounter fileRatingsCRDT = null;
    public GOSet fileVotersCRDT = null;

    public ORset<FileRecord> filesCRDT = null;
    public ORset<String> usersCRDT = null;
}
