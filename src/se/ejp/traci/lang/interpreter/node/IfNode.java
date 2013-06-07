package se.ejp.traci.lang.interpreter.node;

import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.Token;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalArgumentType;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.parser.TraciToken;

public class IfNode implements TraciNode
{
    private final List<TraciNode> condNodes;
    private final List<BlockNode> blockNodes;
    private final TraciToken token;

    public IfNode(final List<TraciNode> condNodes, final List<BlockNode> blockNodes, final Token token)
    {
        this.condNodes = new ArrayList<TraciNode>(condNodes);
        this.blockNodes = new ArrayList<BlockNode>(blockNodes);
        this.token = (TraciToken) token;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        for (int i = 0; i < condNodes.size(); ++i)
        {
            final TraciValue condValue = condNodes.get(i).eval(context);

            if (condValue.getType() != Type.BOOLEAN)
            {
                throw new InterpreterIllegalArgumentType(token.location, context.callStack, "if-statement",
                        Type.BOOLEAN, condValue.getType(), 1);
            }

            if (condValue.getBoolean())
            {
                blockNodes.get(i).eval(context);
                return null;
            }
        }

        if (blockNodes.size() > condNodes.size())
        {
            blockNodes.get(condNodes.size()).eval(context);
        }

        return null;
    }
}
