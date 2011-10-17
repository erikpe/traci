package traci.lang.interpreter.node;

import java.util.List;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
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
    private final List<TraciNode> argNodes;
    private final BlockNode blockNode;
    
    public CsgShapeNode(final String shapeType, final List<TraciNode> argNodes, final BlockNode blockNode)
    {
        this.type = CsgType.valueOf(shapeType);
        this.argNodes = argNodes;
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
        
        if (blockNode != null)
        {
            blockNode.eval(context.newEntity(Entities.makeEntity(csg)));
        }
        
        return new TraciValue(csg);
    }
}
