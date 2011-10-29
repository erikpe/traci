package traci.lang.interpreter.node;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.math.Vector;

public class UnaryOpNode implements TraciNode
{
    private final Op op;
    private final TraciNode aNode;
    private final Token token;

    public UnaryOpNode(final Op op, final TraciNode aNode, final Token token)
    {
        this.op = op;
        this.aNode = aNode;
        this.token = token;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final TraciValue a = aNode.eval(context);
        final TraciValue.Type aType = a.getType();

        final Object res;

        switch (aType)
        {
        case NUMBER:  res = calc(a.getNumber()); break;
        case BOOLEAN: res = calc(a.getBoolean()); break;
        case VECTOR:  res = calc(a.getVector()); break;
        default:      res = null;;
        }

        if (res == null)
        {
            System.out.println("Error: Trying to evaluate expression `" + token.getText() + " " + aType.toString()
                    + "' at position " + token.getLine() + ":" + token.getCharPositionInLine() + ".");
            throw new RuntimeException();
        }

        return new TraciValue(res);
    }

    private Object calc(final Double a)
    {
        switch (op)
        {
        case UNARY_PLUS: return Double.valueOf(a);
        case UNARY_NEG:  return Double.valueOf(-a);
        default: return null;
        }
    }

    private Object calc(final Boolean a)
    {
        switch (op)
        {
        case UNARY_NOT: return Boolean.valueOf(!a);
        default: return null;
        }
    }

    private Object calc(final Vector a)
    {
        switch (op)
        {
        case UNARY_PLUS: return a;
        case UNARY_NEG:  return a.neg();
        default: return null;
        }
    }
}
