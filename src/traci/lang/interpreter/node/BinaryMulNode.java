package traci.lang.interpreter.node;

import traci.lang.interpreter.TraciValue;
import traci.math.Vector;

public class BinaryMulNode extends BinaryOpNode
{
    public BinaryMulNode(final TraciNode aNode, final TraciNode bNode)
    {
        super(aNode, bNode);
    }
    
    protected TraciValue eval(final Double a, final Double b)
    {
        return new TraciValue(Double.valueOf(a * b));
    }
    
    protected TraciValue eval(final Vector a, final Double b)
    {
        return new TraciValue(a.mul(b));
    }
    
    protected TraciValue eval(final Double a, final Vector b)
    {
        return new TraciValue(b.mul(a));
    }
}
