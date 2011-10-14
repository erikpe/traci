package traci.lang.interpreter;

import java.util.HashMap;
import java.util.Map;


public class Context
{
    public final Functions functions;
    public final Map<String, TraciValue> globalMemory;
    public final Map<String, TraciValue> localMemory;
    public final Entity entity;
    
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
        return new Context(new Functions(), new HashMap<String, TraciValue>(), new HashMap<String, TraciValue>(), rootEntity);
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
    
    public TraciValue getValue(final String id)
    {
        final TraciValue localValue = localMemory.get(id);
        
        if (localValue != null)
        {
            return localValue;
        }
        
        return globalMemory.get(id);
    }
}
