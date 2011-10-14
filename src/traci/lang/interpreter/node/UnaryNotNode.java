package traci.lang.interpreter.node;

import traci.lang.interpreter.TraciValue;

public class UnaryNotNode extends UnaryOpNode
{
    public UnaryNotNode(final TraciNode aNode)
    {
        super(aNode);
    }
    
    protected TraciValue eval(final Boolean a)
    {
        return new TraciValue(!a);
    }
}
