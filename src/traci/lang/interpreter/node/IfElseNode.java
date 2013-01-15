package traci.lang.interpreter.node;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.TraciValue.Type;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.parser.TraciToken;

public class IfElseNode implements TraciNode
{
    private final TraciNode condNode;
    private final BlockNode ifBlock;
    private final BlockNode elseBlock;
    private final TraciToken token;

    public IfElseNode(final TraciNode condNode, final BlockNode ifBlock, final BlockNode elseBlock, final Token token)
    {
        this.condNode = condNode;
        this.ifBlock = ifBlock;
        this.elseBlock = elseBlock;
        this.token = (TraciToken) token;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        final TraciValue condValue = condNode.eval(context);

        if (condValue.getType() != Type.BOOLEAN)
        {
            final String msg = "Argument to if()-statement must be " + Type.BOOLEAN.toString() + ", got "
                    + condValue.getType().toString();
            throw new InterpreterRuntimeException(token.location, context.callStack, msg);
        }

        if (condValue.getBoolean())
        {
            ifBlock.eval(context);
        }
        else if (elseBlock != null)
        {
            elseBlock.eval(context);
        }

        return null;
    }
}
