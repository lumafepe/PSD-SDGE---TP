package org.client.crdts.records;

import java.io.Serializable;

public record FileRecord(String name, String hash) implements Serializable {}
