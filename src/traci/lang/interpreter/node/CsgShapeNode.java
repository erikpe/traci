package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.Entity.CsgEntity;
import traci.model.shape.csg.Csg;
import traci.model.shape.csg.Difference;
import traci.model.shape.csg.Intersection;
import traci.model.shape.csg.Union;

public class CsgShapeNode implements TraciNode
{
    private final String shapeType;
    private final BlockNode blockNode;
    
    public CsgShapeNode(final String shapeType, final BlockNode blockNode)
    {
        this.shapeType = shapeType;
        this.blockNode = blockNode;
    }
    
    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final Csg csg;
        
        if (shapeType.equals("union"))
        {
            csg = new Union();
        }
        else if (shapeType.equals("difference"))
        {
            csg = new Difference();
        }
        else if (shapeType.equals("intersection"))
        {
            csg = new Intersection();
        }
        else
        {
            throw new RuntimeException();
        }
        
        if (blockNode != null)
        {
            blockNode.eval(context.newEntity(new CsgEntity(csg)));
        }
        
        return new TraciValue(csg);
    }
}
