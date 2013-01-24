package traci.lang.interpreter;

import java.util.HashMap;
import java.util.Map;

import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.TraciValue.Type;
import traci.lang.interpreter.functions.Function;
import traci.lang.interpreter.functions.FunctionSet;
import traci.lang.parser.IncludeLocation.FileLocation;
import traci.model.Scene;

public class Context
{
    private final Scene scene;
    private final FunctionSet functions;
    private final Map<String, TraciValue> globalMemory;
    private final Map<String, TraciValue> localMemory;
    private final Entity entity;
    public final CallStack callStack;

    private Context(final Scene scene, final FunctionSet functions, final Map<String, TraciValue> globalMemory,
            final Map<String, TraciValue> localMemory, final Entity entity, final CallStack callStack)
    {
        this.scene = scene;
        this.functions = functions;
        this.globalMemory = globalMemory;
        this.localMemory = localMemory;
        this.entity = entity;
        this.callStack = callStack;
    }

    public static Context newRootContext(final Scene scene, final Entity rootEntity)
    {
        return new Context(scene, null, new HashMap<String, TraciValue>(), new HashMap<String, TraciValue>(),
                rootEntity, CallStack.makeEmpty());
    }

    public Context newFuncallContext(final FileLocation location, final String function)
    {
        final CallStack newCallStack = callStack.push(location, function);
        return new Context(scene, null, globalMemory, new HashMap<String, TraciValue>(), Entities.NULL_ENTITY,
                newCallStack);
    }

    public Context newEntity(final Entity newSurroundingEntity)
    {
        return new Context(scene, functions, globalMemory, localMemory, newSurroundingEntity, callStack);
    }

    public Context newFunctions(final FunctionSet newFunctions)
    {
        return new Context(scene, newFunctions, globalMemory, localMemory, entity, callStack);
    }

    public void applyValue(final TraciValue value)
    {
        if (value.getType() == Type.LIGHT)
        {
            scene.addLight(value.getLight());
        }
        else
        {
            entity.applyValue(value);
        }
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
