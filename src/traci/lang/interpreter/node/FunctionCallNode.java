package traci.lang.interpreter.node;

import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.Function;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.interpreter.exceptions.InterpreterUndefinedIdentifier;
import traci.lang.parser.TraciToken;

public class FunctionCallNode implements TraciNode
{
    private final String id;
    private final List<TraciNode> argNodes;
    private final BlockNode blockNode;
    public final TraciToken token;

    public FunctionCallNode(final String id, final List<TraciNode> argNodes, final BlockNode blockNode, final Token token)
    {
        this.id = id;
        this.argNodes = argNodes;
        this.blockNode = blockNode;
        this.token = (TraciToken) token;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        final Function function = context.getFunction(id);
        final List<TraciValue> args = new ArrayList<TraciValue>();

        if (function == null)
        {
            throw new InterpreterUndefinedIdentifier(token.location, context.callStack, "function", id);
        }

        for (final TraciNode argNode : argNodes)
        {
            args.add(argNode.eval(context));
        }

        TraciValue value = function.invoke(this, context, args);

        if (blockNode != null)
        {
            final Entity entity = Entities.makeEntity(value.getObject());
            blockNode.eval(context.newEntity(entity));
            value = entity.getValue();
        }

        return value;
    }

    public TraciToken getToken()
    {
        return token;
    }
}
