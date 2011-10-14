package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.math.Vector;

public abstract class UnaryOpNode implements TraciNode
{
    private final TraciNode aNode;
    
    UnaryOpNode(final TraciNode aNode)
    {
        this.aNode = aNode;
    }
    
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final TraciValue a = aNode.eval(context);
        final TraciValue.Type aType = a.getType();
        
        switch (aType)
        {
        case NUMBER: return eval(a.getNumber());
        case BOOLEAN: return eval(a.getBoolean());
        case VECTOR: return eval(a.getVector());
        default: break;
        }
        
        typeError();
        return null;
    }
    
    protected void typeError()
    {
        throw new RuntimeException("type error");
    }
    
    protected TraciValue eval(final Double a)
    {
        typeError();
        return null;
    }
    
    protected TraciValue eval(final Boolean a)
    {
        typeError();
        return null;
    }
    
    protected TraciValue eval(final Vector a)
    {
        typeError();
        return null;
    }
}
