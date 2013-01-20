package traci.lang.interpreter;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

import traci.lang.interpreter.Entities.Entity;
import traci.lang.parser.IncludeLocation.FileLocation;

public class Context
{
    private final Map<String, TraciValue> globalMemory;
    private final Stack<Map<String, TraciValue>> localMemory;
    private final Stack<Map<String, Function>> functions;
    private final Stack<Entity> entity;
    public final CallStack callStack;

    private Context()
    {
        this.globalMemory = new HashMap<String, TraciValue>();
        this.localMemory = new Stack<Map<String, TraciValue>>();
        this.functions = new Stack<Map<String, Function>>();
        this.entity = new Stack<Entity>();
        this.callStack = new CallStack();
    }

    public static Context newRootContext(final Entity rootEntity)
    {
        final Context context = new Context();
        context.pushEntity(rootEntity);
        context.pushLocalMemory();
        return context;
    }

    public void pushLocalMemory()
    {
        localMemory.push(new HashMap<String, TraciValue>());
    }

    public Map<String, TraciValue> popLocalMemory()
    {
        return localMemory.pop();
    }

    public void pushFunctions(final Map<String, Function> functionMap)
    {
        functions.push(functionMap);
    }

    public Map<String, Function> popFunctions()
    {
        return functions.pop();
    }

    public void pushEntity(final Entity rootEntity)
    {
        entity.push(rootEntity);
    }

    public Entity popEntity()
    {
        return entity.pop();
    }

    public void pushCallStack(final FileLocation location, final String function)
    {
        callStack.push(location, function);
    }

    public void popCallStack()
    {
        callStack.pop();
    }

    public void applyValue(final TraciValue value)
    {
        entity.peek().applyValue(value);
    }

    public Function getFunction(final String id)
    {
        return functions.peek().get(id);
    }

    public void putGlobalValue(final String id, final TraciValue value)
    {
        globalMemory.put(id, value);
    }

    public void putLocalValue(final String id, final TraciValue value)
    {
        localMemory.peek().put(id, value);
    }

    public TraciValue getValue(final String id)
    {
        TraciValue value = localMemory.peek().get(id);

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
