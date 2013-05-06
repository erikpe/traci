package se.ejp.traci.lang.interpreter.node;

import org.antlr.runtime.Token;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalArgumentType;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.parser.TraciToken;

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
                throw new InterpreterIllegalArgumentType(token.location, context.callStack, "while-statement",
                        Type.BOOLEAN, condValue.getType(), 1);
            }

            if (condValue.getBoolean())
            {
                blockNode.eval(context);
            }
        } while (condValue.getBoolean());

        return null;
    }
}
