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
import traci.lang.interpreter.exceptions.InterpreterIllegalNumberOfArguments;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.interpreter.exceptions.InterpreterUndefinedIdentifier;
import traci.lang.parser.TraciToken;

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

    public TraciToken getToken()
    {
        return token;
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
        else if (argNodes.size() != function.numArgs())
        {
            throw new InterpreterIllegalNumberOfArguments(token.location, context.callStack, id, function.numArgs(),
                    argNodes.size());
        }

        for (final TraciNode argNode : argNodes)
        {
            args.add(argNode.eval(context));
        }

        context.pushCallStack(token.location.fileLocation, id);
        context.pushLocalMemory();
        TraciValue value = function.invoke(this, context, args);
        context.popLocalMemory();
        context.popCallStack();

        if (blockNode != null)
        {
            final Entity entity = Entities.makeEntity(value.getObject());
            context.pushEntity(entity);
            try
            {
                blockNode.eval(context);
            }
            catch (final FunctionReturnException e)
            {
                throw e;
            }
            finally
            {
                context.popEntity();
            }
            value = entity.getValue();
        }

        return value;
    }
}
