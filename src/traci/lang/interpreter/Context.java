package traci.lang.interpreter;

import java.util.HashMap;
import java.util.Map;

import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.functions.Function;
import traci.lang.parser.IncludeLocation.FileLocation;

public class Context
{
    private final Map<String, Function> functions;
    private final Map<String, TraciValue> globalMemory;
    private final Map<String, TraciValue> localMemory;
    private final Entity entity;
    public final CallStack callStack;

    private Context(final Map<String, Function> functions, final Map<String, TraciValue> globalMemory,
            final Map<String, TraciValue> localMemory, final Entity entity, final CallStack callStack)
    {
        this.functions = functions;
        this.globalMemory = globalMemory;
        this.localMemory = localMemory;
        this.entity = entity;
        this.callStack = callStack;
    }

    public static Context newRootContext(final Entity rootEntity)
    {
        return new Context(null, new HashMap<String, TraciValue>(), new HashMap<String, TraciValue>(), rootEntity,
                CallStack.makeEmpty());
    }

    public Context newFuncallContext(final FileLocation location, final String function)
    {
        final CallStack newCallStack = callStack.push(location, function);
        return new Context(null, globalMemory, new HashMap<String, TraciValue>(), Entities.NULL_ENTITY, newCallStack);
    }

    public Context newEntity(final Entity newSurroundingEntity)
    {
        return new Context(functions, globalMemory, localMemory, newSurroundingEntity, callStack);
    }

    public Context newFunctions(final Map<String, Function> newFunctions)
    {
        return new Context(newFunctions, globalMemory, localMemory, entity, callStack);
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
