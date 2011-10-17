package traci.lang.interpreter.node;

import java.util.ArrayList;
import java.util.List;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.model.shape.primitive.Box;
import traci.model.shape.primitive.Cylinder;
import traci.model.shape.primitive.Plane;
import traci.model.shape.primitive.Primitive;
import traci.model.shape.primitive.Sphere;
import traci.model.shape.primitive.Torus;

public class PrimitiveShapeNode implements TraciNode
{
    private static enum PrimitiveType
    {
        box,
        cylinder,
        plane,
        sphere,
        torus
    }
    
    private final PrimitiveType type;
    private final List<TraciNode> argNodes;
    private final BlockNode blockNode;
    
    public PrimitiveShapeNode(final String shapeType, final List<TraciNode> argNodes, final BlockNode blockNode)
    {
        this.type = PrimitiveType.valueOf(shapeType);
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
        
        switch (type)
        {
        case box:
            primitive = new Box();
            break;
            
        case cylinder:
            if (args.size() == 3)
            {
                primitive = new Cylinder(args.get(0).getNumber(), args.get(1).getVector(), args.get(2).getVector());
            }
            else
            {
                primitive = new Cylinder();
            }
            break;
            
        case plane:
            primitive = new Plane();
            break;
            
        case sphere:
            primitive = new Sphere();
            break;
            
        case torus:
            primitive = new Torus(args.get(0).getNumber(), args.get(1).getNumber());
            break;
            
        default:
            throw new RuntimeException();
        }
        
        if (blockNode != null)
        {
            blockNode.eval(context.newEntity(Entities.makeEntity(primitive)));
        }
        
        return new TraciValue(primitive);
    }

}