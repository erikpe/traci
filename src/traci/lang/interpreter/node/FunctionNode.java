package traci.lang.interpreter.node;

import java.util.List;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
import traci.lang.interpreter.Function;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;

public class FunctionNode implements TraciNode, Function
{
    public final String id;
    public final List<String> argIDs;
    private final BlockNode bodyNode;
    
    public FunctionNode(final String id, final List<String> argIDs, final BlockNode bodyNode)
    {
        this.id = id;
        this.argIDs = argIDs;
        this.bodyNode = bodyNode;
    }
    
    @Override
    public TraciValue eval(Context dummy)
    {
        throw new RuntimeException();
    }
    
    public TraciValue invoke(Context context, final List<TraciValue> args)
    {
        context = context.newLocalMemory().newEntity(Entities.NULL_ENTITY);
        
        if (args.size() < argIDs.size())
        {
            throw new RuntimeException("too few arguments");
        }
        
        if (args.size() > argIDs.size())
        {
            throw new RuntimeException("too many arguments");
        }
        
        for (int i = 0; i < argIDs.size(); ++i)
        {
            context.putLocalValue(argIDs.get(i), args.get(i));
        }
        
        try
        {
            bodyNode.eval(context);
        }
        catch (final FunctionReturnException e)
        {
            return e.value;
        }
        
        return null;
    }
}
