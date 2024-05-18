package org.client.crdts;

import org.client.crdts.records.FileRecord;
import java.io.Serializable;
import java.util.Map;

public class CRDTS implements Serializable {

    public GCounter fileRatingsCRDT = null;
    public Map<String, GOSet> fileVotersCRDT = null;

    public ORset<FileRecord> filesCRDT = null;
    public ORset<String> usersCRDT = null;
}
