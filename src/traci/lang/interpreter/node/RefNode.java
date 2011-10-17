package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;

public class RefNode implements TraciNode
{
    private final String id;
    private final BlockNode blockNode;
    
    public RefNode(final String id, final BlockNode blockNode)
    {
        this.id = id;
        this.blockNode = blockNode;
    }
    
    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final TraciValue value = context.getValue(id);
        
        if (blockNode != null)
        {
            blockNode.eval(context.newEntity(Entities.makeEntity(value.getValue())));
        }
        
        return value;
    }
}
