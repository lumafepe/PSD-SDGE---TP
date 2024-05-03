package org.example.CRDTs;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class ORset {
    private HashMap<Object, Set<VectorClock>> map;
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

    public Operation addElement(String operationName, String element, String id){
        counter += 1;

        Operation operation = new Operation(operationName, element, new VectorClock(id, counter), map.get(element));
        applyAddOperation(operation);

        return operation;
    }

    public void applyAddOperation(Operation addOperation){
        Set<VectorClock> oldValue = map.get(addOperation.element);
        if (oldValue == null){
            oldValue = new HashSet<>();
        }

        if (addOperation.observed != null)
            for (Object toRemove : addOperation.observed)
                oldValue.remove(toRemove);

        oldValue.add(addOperation.vectorClock);
        map.put(addOperation.element, oldValue);
    }

    public Operation removeElement(String element){
        Operation operation = new Operation("remove", element, null, map.get(element));
        applyRemoveOperation(operation);
        return operation;
    }

    public void applyRemoveOperation(Operation removeOperation){
        Set<VectorClock> oldValue = map.get(removeOperation.element);
        for (Object toRemove : removeOperation.observed)
            oldValue.remove(toRemove);

        map.put(removeOperation.element, oldValue);
    }
}
