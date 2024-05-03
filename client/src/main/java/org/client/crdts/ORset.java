package org.client.crdts;

import java.util.HashMap;
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

    public Operation addElement(Object element){
        counter += 1;

        long pid = ProcessHandle.current().pid();
        Operation operation = new Operation("add", element, new VectorClock(pid, counter), map.get(element));
        applyAddOperation(operation);

        return operation;
    }

    public void applyAddOperation(Operation addOperation){
        Set<VectorClock> oldValue = map.get(addOperation.element);

        for (Object toRemove : addOperation.observed)
            oldValue.remove(toRemove);

        oldValue.add(addOperation.vectorClock);
        map.put(addOperation.element, oldValue);
    }

    public Operation removeElement(Object element){
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
