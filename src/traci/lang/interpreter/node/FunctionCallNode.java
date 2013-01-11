package traci.lang.interpreter.node;

import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.Token;

import traci.lang.grammar.TraciToken;
import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.Function;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.util.Log;

public class FunctionCallNode implements TraciNode
{
    private final String id;
    private final List<TraciNode> argNodes;
    private final BlockNode blockNode;
    private final TraciToken token;

    public FunctionCallNode(final String id, final List<TraciNode> argNodes, final BlockNode blockNode, final Token token)
    {
        this.id = id;
        this.argNodes = argNodes;
        this.blockNode = blockNode;
        this.token = (TraciToken) token;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final Function functionNode = context.getFunction(id);
        final List<TraciValue> args = new ArrayList<TraciValue>();

        if (functionNode == null)
        {
            Log.ERROR(token.location.toString());
            Log.ERROR("No such function defined: '" + id + "()'");
            System.exit(-1);
        }

        for (final TraciNode argNode : argNodes)
        {
            args.add(argNode.eval(context));
        }

        TraciValue value = functionNode.invoke(context.newFuncallContext(token.location.fileLocation, id), args);

        if (blockNode != null)
        {
            final Entity entity = Entities.makeEntity(value.getObject());
            blockNode.eval(context.newEntity(entity));
            value = entity.getValue();
        }

        return value;
    }
}
