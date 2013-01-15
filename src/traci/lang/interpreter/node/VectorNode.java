package traci.lang.interpreter.node;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.TraciValue.Type;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.parser.TraciToken;
import traci.math.Vector;

public class VectorNode implements TraciNode
{
    private final TraciNode aNode;
    private final TraciNode bNode;
    private final TraciNode cNode;
    private final TraciToken token;

    public VectorNode(final TraciNode aNode, final TraciNode bNode, final TraciNode cNode, final Token token)
    {
        this.aNode = aNode;
        this.bNode = bNode;
        this.cNode = cNode;
        this.token = (TraciToken) token;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        final TraciValue a = aNode.eval(context);

        if (a.getType() != Type.NUMBER)
        {
            final String msg = "First argument to vector-expression must be " + Type.NUMBER.toString() + ", got "
                    + a.getType().toString();
            throw new InterpreterRuntimeException(token.location, context.callStack, msg);
        }

        final TraciValue b = bNode.eval(context);

        if (b.getType() != Type.NUMBER)
        {
            final String msg = "Second argument to vector-expression must be " + Type.NUMBER.toString() + ", got "
                    + b.getType().toString();
            throw new InterpreterRuntimeException(token.location, context.callStack, msg);
        }

        final TraciValue c = cNode.eval(context);

        if (c.getType() != Type.NUMBER)
        {
            final String msg = "Third argument to vector-expression must be " + Type.NUMBER.toString() + ", got "
                    + c.getType().toString();
            throw new InterpreterRuntimeException(token.location, context.callStack, msg);
        }

        return new TraciValue(Vector.make(a.getNumber(), b.getNumber(), c.getNumber()));
    }
}
