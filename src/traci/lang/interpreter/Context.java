package traci.lang.interpreter;

import java.util.HashMap;
import java.util.Map;

public class Context
{
    private final Functions functions;
    private final Map<String, TraciValue> globalMemory;
    private final Map<String, TraciValue> localMemory;
    private final Entity entity;
    
    private Context(final Functions functions, final Map<String, TraciValue> globalMemory,
            final Map<String, TraciValue> localMemory, final Entity entity)
    {
        this.functions = functions;
        this.globalMemory = globalMemory;
        this.localMemory = localMemory;
        this.entity = entity;
    }
    
    public static Context newRootContext(final Entity rootEntity)
    {
        return new Context(null, new HashMap<String, TraciValue>(), new HashMap<String, TraciValue>(), rootEntity);
    }
    
    public Context newLocalMemory()
    {
        return new Context(functions, globalMemory, new HashMap<String, TraciValue>(), entity);
    }
    
    public Context newEntity(final Entity newSurroundingEntity)
    {
        return new Context(functions, globalMemory, localMemory, newSurroundingEntity);
    }
    
    public Context newFunctions(final Functions newFunctions)
    {
        return new Context(newFunctions, globalMemory, localMemory, entity);
    }
    
    public void applyValue(final TraciValue value)
    {
        entity.applyValue(value);
    }
    
    public Function getFunction(final String id)
    {
        return functions.get(id);
    }
    
    public void putGlobalValue(final String id, final TraciValue value)
    {
        globalMemory.put(id, value);
    }
    
    public void putLocalValue(final String id, final TraciValue value)
    {
        localMemory.put(id, value);
    }
    
    public TraciValue getValue(final String id)
    {
        TraciValue value = localMemory.get(id);
        
        if (value == null)
        {
            value = globalMemory.get(id);
        }
        
        if (value == null)
        {
            return null;
        }
        
        return (TraciValue) value.clone();
    }
}
