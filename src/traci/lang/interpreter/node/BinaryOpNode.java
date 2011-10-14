package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.math.Vector;

public abstract class BinaryOpNode implements TraciNode
{
    private final TraciNode aNode;
    private final TraciNode bNode;
    
    BinaryOpNode(final TraciNode aNode, final TraciNode bNode)
    {
        this.aNode = aNode;
        this.bNode = bNode;
    }
    
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final TraciValue a = aNode.eval(context);
        final TraciValue b = bNode.eval(context);
        
        final TraciValue.Type aType = a.getType();
        final TraciValue.Type bType = b.getType();
        
        switch (aType)
        {
        case NUMBER:
            switch (bType)
            {
            case NUMBER: return eval(a.getNumber(), b.getNumber());
            case VECTOR: return eval(a.getNumber(), b.getVector());
            default: break;
            }
            
        case VECTOR:
            switch (bType)
            {
            case NUMBER: return eval(a.getVector(), b.getNumber());
            case VECTOR: return eval(a.getVector(), b.getVector());
            default: break;
            }
            
        case BOOLEAN:
            switch (bType)
            {
            case BOOLEAN: return eval(a.getBoolean(), b.getBoolean());
            default : break;
            }
            
        default:
            break;
        }
        
        typeError();
        return null;
    }
    
    protected void typeError()
    {
        throw new RuntimeException("type error");
    }
    
    protected TraciValue eval(final Double a, final Double b)
    {
        typeError();
        return null;
    }
    
    protected TraciValue eval(final Double a, final Vector b)
    {
        typeError();
        return null;
    }
    
    protected TraciValue eval(final Vector a, final Vector b)
    {
        typeError();
        return null;
    }
    
    protected TraciValue eval(final Vector a, final Double b)
    {
        typeError();
        return null;
    }
    
    protected TraciValue eval(final Boolean a, final Boolean b)
    {
        typeError();
        return null;
    }
}
