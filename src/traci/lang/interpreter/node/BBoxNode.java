package traci.lang.interpreter.node;

import java.util.List;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entity.BBoxEntity;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.model.shape.BoundingBox;

public class BBoxNode implements TraciNode
{
    private final List<TraciNode> argNodes;
    private final BlockNode blockNode;
    
    public BBoxNode(final List<TraciNode> argNodes, final BlockNode blockNode)
    {
        this.argNodes = argNodes;
        this.blockNode = blockNode;
    }
    
    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final BoundingBox bBox = new BoundingBox();
        
        if (blockNode != null)
        {
            blockNode.eval(context.newEntity(new BBoxEntity(bBox)));
        }
        
        return new TraciValue(bBox);
    }
}
