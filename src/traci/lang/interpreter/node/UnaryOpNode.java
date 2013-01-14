package traci.lang.interpreter.node;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.InterpreterRuntimeException;
import traci.lang.interpreter.TraciValue;
import traci.lang.parser.TraciToken;
import traci.math.Vector;

public class UnaryOpNode implements TraciNode
{
    private final Op op;
    private final TraciNode aNode;
    private final TraciToken token;

    public UnaryOpNode(final Op op, final TraciNode aNode, final Token token)
    {
        this.op = op;
        this.aNode = aNode;
        this.token = (TraciToken) token;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
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
            final String msg = "Unable to evaluate expression '" + token.getText() + " " + aType.toString() + "'";
            throw new InterpreterRuntimeException(token.location, msg, context.callStack);
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
