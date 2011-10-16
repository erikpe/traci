package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.TraciValue.Type;
import traci.math.Transformation;
import traci.math.Transformations;

public class TransformationNode implements TraciNode
{
    private final String type;
    private final TraciNode exprNode;
    
    public TransformationNode(final String type, final TraciNode exprNode)
    {
        this.type = type;
        this.exprNode = exprNode;
    }
    
    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final TraciValue exprValue = exprNode.eval(context);
        final Transformation transformation;
        
        if (type.equals("rotx"))
        {
            transformation = Transformations.rotx(exprValue.getNumber());
        }
        else if (type.equals("roty"))
        {
            transformation = Transformations.roty(exprValue.getNumber());
        }
        else if (type.equals("rotz"))
        {
            transformation = Transformations.rotz(exprValue.getNumber());
        }
        else if (type.equals("translate"))
        {
            transformation = Transformations.translate(exprValue.getVector());
        }
        else if (type.equals("scale") && exprValue.getType() == Type.NUMBER)
        {
            transformation = Transformations.scale(exprValue.getNumber());
        }
        else if (type.equals("scale") && exprValue.getType() == Type.VECTOR)
        {
            transformation = Transformations.scale(exprValue.getVector());
        }
        else
        {
            throw new RuntimeException();
        }
        
        return new TraciValue(transformation);
    }
}
