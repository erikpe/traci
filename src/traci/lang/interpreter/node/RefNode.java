package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entity;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.TraciValue.Type;

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
            final Entity entity;
            
            if (value.getType() == Type.PRIMITIVE_SHAPE)
            {
                entity = new Entity.PrimitiveEntity(value.getPrimitive());
            }
            else if (value.getType() == Type.CSG_SHAPE)
            {
                entity = new Entity.CsgEntity(value.getCsg());
            }
            else if (value.getType() == Type.BOUNDING_BOX)
            {
                entity = new Entity.BBoxEntity(value.getBoundingBox());
            }
            else
            {
                throw new RuntimeException();
            }
            
            blockNode.eval(context.newEntity(entity));
        }
        
        return value;
    }
}
