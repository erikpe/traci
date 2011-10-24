package traci.lang.interpreter.node;

import java.util.ArrayList;
import java.util.List;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.Function;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;

public class FunctionCallNode implements TraciNode
{
    private final String id;
    private final List<TraciNode> argNodes;
    private final BlockNode blockNode;
    
    public FunctionCallNode(final String id, final List<TraciNode> argNodes, final BlockNode blockNode)
    {
        this.id = id;
        this.argNodes = argNodes;
        this.blockNode = blockNode;
    }
    
    @Override
    public TraciValue eval(Context context) throws FunctionReturnException
    {
        final Function functionNode = context.getFunction(id);
        final List<TraciValue> args = new ArrayList<TraciValue>();
        
        for (final TraciNode argNode : argNodes)
        {
            args.add(argNode.eval(context));
        }
        
        TraciValue value = functionNode.invoke(context, args);
        
        if (blockNode != null)
        {
            final Entity entity = Entities.makeEntity(value.getObject());
            blockNode.eval(context.newEntity(entity));
            value = entity.getValue();
        }
        
        return value;
    }
}
