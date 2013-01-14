package traci.lang.interpreter.node;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.InterpreterRuntimeException;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.TraciValue.Type;
import traci.lang.parser.TraciToken;

public class WhileNode implements TraciNode
{
    private final TraciNode condNode;
    private final BlockNode blockNode;
    private final TraciToken token;

    public WhileNode(final TraciNode condNode, final BlockNode blockNode, final Token token)
    {
        this.condNode = condNode;
        this.blockNode = blockNode;
        this.token = (TraciToken) token;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        TraciValue condValue;

        do
        {
            condValue = condNode.eval(context);

            if (condValue.getType() != Type.BOOLEAN)
            {
                final String msg = "Argument to while()-statement must be " + Type.BOOLEAN.toString() + ", got "
                        + condValue.getType().toString();
                throw new InterpreterRuntimeException(token.location, msg, context.callStack);
            }

            if (condValue.getBoolean())
            {
                blockNode.eval(context);
            }
        } while (condValue.getBoolean());

        return null;
    }
}
