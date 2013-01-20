package traci.lang.interpreter.node;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.interpreter.exceptions.InterpreterUndefinedIdentifier;
import traci.lang.parser.TraciToken;

public class RefNode implements TraciNode
{
    private final String id;
    private final BlockNode blockNode;
    private final TraciToken token;

    public RefNode(final String id, final BlockNode blockNode, final Token token)
    {
        this.id = id;
        this.blockNode = blockNode;
        this.token = (TraciToken) token;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        TraciValue value = context.getValue(id);

        if (value == null)
        {
            throw new InterpreterUndefinedIdentifier(token.location, context.callStack, "variable", id);
        }

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
