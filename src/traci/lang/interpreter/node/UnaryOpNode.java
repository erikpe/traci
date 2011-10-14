package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.math.Vector;

public class UnaryOpNode implements TraciNode
{
    private final Op op;
    private final TraciNode aNode;
    
    public UnaryOpNode(final Op op, final TraciNode aNode)
    {
        this.op = op;
        this.aNode = aNode;
    }
    
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final TraciValue a = aNode.eval(context);
        final TraciValue.Type aType = a.getType();
        
        switch (aType)
        {
        case NUMBER:  return calc(a.getNumber());
        case BOOLEAN: return calc(a.getBoolean());
        case VECTOR:  return calc(a.getVector());
        default: break;
        }
        
        throw new RuntimeException("type error");
    }
    
    private TraciValue calc(final Double a)
    {
        final Double res;
        
        switch (op)
        {
        case UNARY_PLUS: res = Double.valueOf(a); break;
        case UNARY_NEG:  res = Double.valueOf(-a); break;
        default: throw new RuntimeException("type error");
        }
        
        return new TraciValue(res);
    }
    
    private TraciValue calc(final Boolean a)
    {
        final Boolean res;
        
        switch (op)
        {
        case UNARY_NOT: res = Boolean.valueOf(!a); break;
        default: throw new RuntimeException("type error");
        }
        
        return new TraciValue(res);
    }
    
    private TraciValue calc(final Vector a)
    {
        final Vector res;
        
        switch (op)
        {
        case UNARY_PLUS: res = a; break;
        case UNARY_NEG:  res = a.neg(); break;
        default: throw new RuntimeException("type error");
        }
        
        return new TraciValue(res);
    }
}
