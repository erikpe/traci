package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.math.Vector;

public class BinaryOpNode implements TraciNode
{
    private final Op op;
    private final TraciNode aNode;
    private final TraciNode bNode;
    
    public BinaryOpNode(final Op op, final TraciNode aNode, final TraciNode bNode)
    {
        this.op = op;
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
            case NUMBER: return calc(a.getNumber(), b.getNumber());
            case VECTOR: return calc(a.getNumber(), b.getVector());
            default: break;
            }
            
        case VECTOR:
            switch (bType)
            {
            case NUMBER: return calc(a.getVector(), b.getNumber());
            case VECTOR: return calc(a.getVector(), b.getVector());
            default: break;
            }
            
        case BOOLEAN:
            switch (bType)
            {
            case BOOLEAN: return calc(a.getBoolean(), b.getBoolean());
            default : break;
            }
            
        default:
            break;
        }
        
        throw new RuntimeException("type error");
    }
    
    private TraciValue calc(final Double a, final Double b)
    {
        final Object res;
        
        switch (op)
        {
        case BINARY_ADD:  res = Double.valueOf(a + b); break;
        case BINARY_SUB:  res = Double.valueOf(a - b); break;
        case BINARY_MUL:  res = Double.valueOf(a * b); break;
        case BINARY_DIV:  res = Double.valueOf(a / b); break;
        case COMPARE_LT:  res = Boolean.valueOf(a < b); break;
        case COMPARE_LTE: res = Boolean.valueOf(a <= b); break;
        case COMPARE_GT:  res = Boolean.valueOf(a > b); break;
        case COMPARE_GTE: res = Boolean.valueOf(a >= b); break;
        case COMPARE_EQ:  res = Boolean.valueOf(a == b); break;
        case COMPARE_NEQ: res = Boolean.valueOf(a != b); break;
        default: throw new RuntimeException("type error");
        }
        
        return new TraciValue(res);
    }
    
    private TraciValue calc(final Double a, final Vector b)
    {
        final Vector res;
        
        switch (op)
        {
        case BINARY_MUL: res = b.mul(a); break;
        default: throw new RuntimeException("type error");
        }
        
        return new TraciValue(res);
    }
    
    private TraciValue calc(final Vector a, final Double b)
    {
        final Vector res;
        
        switch (op)
        {
        case BINARY_MUL: res = a.mul(b); break;
        default: throw new RuntimeException("type error");
        }
        
        return new TraciValue(res);
    }
    
    private TraciValue calc(final Vector a, final Vector b)
    {
        final Vector res;
        
        switch (op)
        {
        case BINARY_ADD: res = a.add(b); break;
        case BINARY_SUB: res = a.sub(b); break;
        default: throw new RuntimeException("type error");
        }
        
        return new TraciValue(res);
    }
    
    private TraciValue calc(final Boolean a, final Boolean b)
    {
        final Boolean res;
        
        switch (op)
        {
        case COMPARE_EQ:  res = Boolean.valueOf(a == b); break;
        case COMPARE_NEQ: res = Boolean.valueOf(a != b); break;
        default: throw new RuntimeException("type error");
        }
        
        return new TraciValue(res);
    }
}
