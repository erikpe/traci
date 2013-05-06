package se.ejp.traci.lang.interpreter.node;

import org.antlr.runtime.Token;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalArgumentType;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.parser.TraciToken;

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
            throw new InterpreterIllegalArgumentType(token.location, context.callStack, "if-statement",
                    Type.BOOLEAN, condValue.getType(), 1);
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