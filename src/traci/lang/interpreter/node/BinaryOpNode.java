package traci.lang.interpreter.node;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.math.Vector;

public class BinaryOpNode implements TraciNode
{
    private final Op op;
    private final TraciNode aNode;
    private final TraciNode bNode;
    private final Token token;

    public BinaryOpNode(final Op op, final TraciNode aNode, final TraciNode bNode, final Token token)
    {
        this.op = op;
        this.aNode = aNode;
        this.bNode = bNode;
        this.token = token;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final TraciValue a = aNode.eval(context);
        final TraciValue b = bNode.eval(context);

        final TraciValue.Type aType = a.getType();
        final TraciValue.Type bType = b.getType();

        final Object res;

        switch (aType)
        {
        case NUMBER:
            switch (bType)
            {
            case NUMBER: res = calc(a.getNumber(), b.getNumber()); break;
            case VECTOR: res = calc(a.getNumber(), b.getVector()); break;
            default:     res = null; break;
            }
            break;

        case VECTOR:
            switch (bType)
            {
            case NUMBER: res = calc(a.getVector(), b.getNumber()); break;
            case VECTOR: res = calc(a.getVector(), b.getVector()); break;
            default:     res = null; break;
            }
            break;

        case BOOLEAN:
            switch (bType)
            {
            case BOOLEAN: res = calc(a.getBoolean(), b.getBoolean()); break;
            default :     res = null; break;
            }
            break;

        default:
            res = null;
            break;
        }

        if (res == null)
        {
            System.out.println("Error: Trying to evaluate expression `" + aType.toString() + " " + token.getText() + " "
                    + bType.toString() + "' at position " + token.getLine() + ":" + token.getCharPositionInLine() + ".");
            throw new RuntimeException();
        }

        return new TraciValue(res);
    }

    private Object calc(final Double a, final Double b)
    {
        switch (op)
        {
        case BINARY_ADD:  return Double.valueOf(a + b);
        case BINARY_SUB:  return Double.valueOf(a - b);
        case BINARY_MUL:  return Double.valueOf(a * b);
        case BINARY_DIV:  return Double.valueOf(a / b);
        case COMPARE_LT:  return Boolean.valueOf(a < b);
        case COMPARE_LTE: return Boolean.valueOf(a <= b);
        case COMPARE_GT:  return Boolean.valueOf(a > b);
        case COMPARE_GTE: return Boolean.valueOf(a >= b);
        case COMPARE_EQ:  return Boolean.valueOf(a == b);
        case COMPARE_NEQ: return Boolean.valueOf(a != b);
        default: return null;
        }
    }

    private Object calc(final Double a, final Vector b)
    {
        switch (op)
        {
        case BINARY_MUL: return b.mul(a);
        default: return null;
        }
    }

    private Object calc(final Vector a, final Double b)
    {
        switch (op)
        {
        case BINARY_MUL: return a.mul(b);
        default: return null;
        }
    }

    private Object calc(final Vector a, final Vector b)
    {
        switch (op)
        {
        case BINARY_ADD: return a.add(b);
        case BINARY_SUB: return a.sub(b);
        default: return null;
        }
    }

    private Object calc(final Boolean a, final Boolean b)
    {
        switch (op)
        {
        case COMPARE_EQ:  return Boolean.valueOf(a == b);
        case COMPARE_NEQ: return Boolean.valueOf(a != b);
        default: return null;
        }
    }
}
