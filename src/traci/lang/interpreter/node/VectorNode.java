package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.TraciValue.Type;
import traci.math.Vector;

public class VectorNode implements TraciNode
{
    private final TraciNode aNode;
    private final TraciNode bNode;
    private final TraciNode cNode;

    public VectorNode(final TraciNode aNode, final TraciNode bNode, final TraciNode cNode)
    {
        this.aNode = aNode;
        this.bNode = bNode;
        this.cNode = cNode;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final TraciValue a = aNode.eval(context);
        final TraciValue b = bNode.eval(context);
        final TraciValue c = cNode.eval(context);

        if (a.getType() != Type.NUMBER || b.getType() != Type.NUMBER || c.getType() != Type.NUMBER)
        {
            throw new RuntimeException("type error");
        }

        return new TraciValue(Vector.make(a.getNumber(), b.getNumber(), c.getNumber()));
    }
}
