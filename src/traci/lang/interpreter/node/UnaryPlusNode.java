package traci.lang.interpreter.node;

import traci.lang.interpreter.TraciValue;
import traci.math.Vector;

public class UnaryPlusNode extends UnaryOpNode
{
    public UnaryPlusNode(final TraciNode aNode)
    {
        super(aNode);
    }
    
    protected TraciValue eval(final Double a)
    {
        return new TraciValue(a);
    }
    
    protected TraciValue eval(final Vector a)
    {
        return new TraciValue(a);
    }
}
