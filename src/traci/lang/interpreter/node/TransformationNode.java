package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.TraciValue.Type;
import traci.math.Transformation;
import traci.math.Transformations;

public class TransformationNode implements TraciNode
{
    private static enum TransformationType
    {
        rotx,
        roty,
        rotz,
        translate,
        scale
    }

    private final TransformationType type;
    private final TraciNode exprNode;

    public TransformationNode(final String typeStr, final TraciNode exprNode)
    {
        this.type = TransformationType.valueOf(typeStr);
        this.exprNode = exprNode;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final TraciValue exprValue = exprNode.eval(context);
        final Transformation transformation;

        switch (type)
        {
        case rotx:
            transformation = Transformations.rotx(exprValue.getNumber());
            break;

        case roty:
            transformation = Transformations.roty(exprValue.getNumber());
            break;

        case rotz:
            transformation = Transformations.rotz(exprValue.getNumber());
            break;

        case translate:
            transformation = Transformations.translate(exprValue.getVector());
            break;

        case scale:
            if (exprValue.getType() == Type.NUMBER)
            {
                transformation = Transformations.scale(exprValue.getNumber());
            }
            else
            {
                transformation = Transformations.scale(exprValue.getVector());
            }
            break;

        default:
            throw new RuntimeException();
        }

        return new TraciValue(transformation);
    }
}
