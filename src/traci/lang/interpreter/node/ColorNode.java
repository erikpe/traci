package traci.lang.interpreter.node;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.TraciValue.Type;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.parser.TraciToken;
import traci.model.material.Color;
import traci.model.material.pigment.Solid;

public class ColorNode implements TraciNode
{
    private final TraciNode exprNode;
    private final TraciToken token;

    public ColorNode(final TraciNode exprNode, final Token token)
    {
        this.exprNode = exprNode;
        this.token = (TraciToken) token;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        final TraciValue exprValue = exprNode.eval(context);

        if (exprValue.getType() != Type.VECTOR)
        {
            final String msg = "Argument to color-statement must be " + Type.VECTOR.toString() + ", got "
                    + exprValue.getType().toString();
            throw new InterpreterRuntimeException(token.location, context.callStack, msg);
        }

        final Color color = Color.make(exprValue.getVector());

        return new TraciValue(Solid.make(color));
    }
}
