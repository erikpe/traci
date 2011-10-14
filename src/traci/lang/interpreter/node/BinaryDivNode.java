package traci.lang.interpreter.node;

import traci.lang.interpreter.TraciValue;
import traci.math.Vector;

public class BinaryDivNode extends BinaryOpNode
{
    public BinaryDivNode(final TraciNode aNode, final TraciNode bNode)
    {
        super(aNode, bNode);
    }
    
    protected TraciValue eval(final Double a, final Double b)
    {
        return new TraciValue(Double.valueOf(a + b));
    }
    
    protected TraciValue eval(final Vector a, final Vector b)
    {
        return new TraciValue(a.add(b));
    }
}
