package traci.lang.interpreter.node;

import java.util.ArrayList;
import java.util.List;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.Entity.PrimitiveEntity;
import traci.model.shape.primitive.Box;
import traci.model.shape.primitive.Cylinder;
import traci.model.shape.primitive.Plane;
import traci.model.shape.primitive.Primitive;
import traci.model.shape.primitive.Sphere;
import traci.model.shape.primitive.Torus;

public class PrimitiveShapeNode implements TraciNode
{
    private final String shapeType;
    private final List<TraciNode> argNodes;
    private final BlockNode blockNode;
    
    public PrimitiveShapeNode(final String shapeType, final List<TraciNode> argNodes, final BlockNode blockNode)
    {
        this.shapeType = shapeType;
        this.argNodes = argNodes;
        this.blockNode = blockNode;
    }
    
    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final List<TraciValue> args = new ArrayList<TraciValue>();
        
        if (argNodes != null)
        {
            for (final TraciNode argNode : argNodes)
            {
                args.add(argNode.eval(context));
            }
        }
        
        final Primitive primitive;
        
        if (shapeType.equals("box"))
        {
            primitive = new Box();
        }
        else if (shapeType.equals("cylinder"))
        {
            if (args.size() == 3)
            {
                primitive = new Cylinder(args.get(0).getNumber(), args.get(1).getVector(), args.get(2).getVector());
            }
            else
            {
                primitive = new Cylinder();
            }
        }
        else if (shapeType.equals("plane"))
        {
            primitive = new Plane();
        }
        else if (shapeType.equals("sphere"))
        {
            primitive = new Sphere();
        }
        else if (shapeType.equals("torus"))
        {
            primitive = new Torus(args.get(0).getNumber(), args.get(0).getNumber());
        }
        else
        {
            throw new RuntimeException();
        }
        
        if (blockNode != null)
        {
            blockNode.eval(context.newEntity(new PrimitiveEntity(primitive)));
        }
        
        return new TraciValue(primitive);
    }

}
