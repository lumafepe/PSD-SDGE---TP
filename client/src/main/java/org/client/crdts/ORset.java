package org.client.crdts;

import org.client.crdts.base.Operation;
import org.client.crdts.base.VersionVector;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class ORset {
    private HashMap<Object, Set<VersionVector>> map;
    private int counter;

    public ORset(){
        map = new HashMap<>();
        counter = 0;
    }

    public Set<Object> elements(){
        return map.keySet();
    }

    public boolean contains(Object element){
        return map.containsKey(element);
    }

    public void insert(Object element, VersionVector vector){
        HashSet<VersionVector> set = new HashSet<VersionVector>();
        set.add(vector);
        this.map.put(element, set);
    }

    public Operation addElement(String operationName, String element, String id){
        counter += 1;

        Operation operation = new Operation(operationName, element, new VersionVector(id, counter), map.get(element));
        applyAddOperation(operation);

        return operation;
    }

    public void applyAddOperation(Operation addOperation){
        Set<VersionVector> oldValue = map.get(addOperation.element);
        if (oldValue == null){
            oldValue = new HashSet<>();
        }

        if (addOperation.observed != null)
            for (Object toRemove : addOperation.observed)
                oldValue.remove(toRemove);

        oldValue.add(addOperation.versionVector);
        map.put(addOperation.element, oldValue);
    }

    public Operation removeElement(String operationName, String element){
        Operation operation = new Operation(operationName, element, null, map.get(element));
        applyRemoveOperation(operation);
        return operation;
    }

    public void applyRemoveOperation(Operation removeOperation){
        Set<VersionVector> oldValue = map.get(removeOperation.element);
        if (oldValue == null){
            oldValue = new HashSet<>();
        }
        if (removeOperation.observed != null)
            for (Object toRemove : removeOperation.observed)
                oldValue.remove(toRemove);

        map.put(removeOperation.element, oldValue);
        if (oldValue.size() == 0){
            map.remove(removeOperation.element);
        }
    }

    public String toString(){
        return map.toString();
    }
}
