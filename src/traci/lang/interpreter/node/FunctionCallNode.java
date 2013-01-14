package traci.lang.interpreter.node;

import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.Function;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.lang.parser.TraciToken;
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
        final Function function = context.getFunction(id);
        final List<TraciValue> args = new ArrayList<TraciValue>();

        if (function == null)
        {
            Log.ERROR(token.location.toString());
            Log.ERROR("Runtime error: No such function: '" + id + "()'\n"
                    + context.callStack.print(token.location.fileLocation));
            System.exit(-1);
        }
        else if (argNodes.size() != function.numArgs())
        {
            final boolean tooFew = args.size() < function.numArgs();
            final String amount = (tooFew ? "few" : "many");
            Log.ERROR(token.location.toString());
            Log.ERROR("Runtime error: Too " +  amount + " argumentssuch function: '" + id + "()'\n"
                    + context.callStack.print(token.location.fileLocation));
            System.exit(-1);
        }

        if (function == null)
        {
            Log.ERROR(token.location.toString());
            Log.ERROR("No such function defined: '" + id + "()'");
            System.exit(-1);
        }

        for (final TraciNode argNode : argNodes)
        {
            args.add(argNode.eval(context));
        }

        TraciValue value = function.invoke(context.newFuncallContext(token.location.fileLocation, id), args);

        if (blockNode != null)
        {
            final Entity entity = Entities.makeEntity(value.getObject());
            blockNode.eval(context.newEntity(entity));
            value = entity.getValue();
        }

        return value;
    }
}
