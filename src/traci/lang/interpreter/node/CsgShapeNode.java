package traci.lang.interpreter.node;

import java.util.List;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.model.shape.csg.Csg;
import traci.model.shape.csg.Difference;
import traci.model.shape.csg.Intersection;
import traci.model.shape.csg.Union;

public class CsgShapeNode implements TraciNode
{
    private static enum CsgType
    {
        union,
        difference,
        intersection
    }
    
    private final CsgType type;
    private final BlockNode blockNode;
    
    public CsgShapeNode(final String shapeType, final List<TraciNode> argNodes, final BlockNode blockNode)
    {
        this.type = CsgType.valueOf(shapeType);
        this.blockNode = blockNode;
    }
    
    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final Csg csg;
        
        switch (type)
        {
        case union:
            csg = new Union();
            break;
            
        case difference:
            csg = new Difference();
            break;
            
        case intersection:
            csg = new Intersection();
            break;
            
        default:
            throw new RuntimeException();
        }
        
        TraciValue value = new TraciValue(csg);
        
        if (blockNode != null)
        {
            final Entity entity = Entities.makeEntity(csg);
            blockNode.eval(context.newEntity(entity));
            value = entity.getValue();
            assert csg == value.getObject();
        }
        
        return value;
    }
}
