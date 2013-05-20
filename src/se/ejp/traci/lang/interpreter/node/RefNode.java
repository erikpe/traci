package se.ejp.traci.lang.interpreter.node;

import org.antlr.runtime.Token;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.Entities;
import se.ejp.traci.lang.interpreter.Entities.Entity;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterInternalException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterUndefinedIdentifier;
import se.ejp.traci.lang.parser.TraciToken;

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
        TraciValue value;

        try
        {
            value = context.getValue(id);
        }
        catch (final CloneNotSupportedException e)
        {
            throw new InterpreterInternalException("Unable to clone object");
        }

        if (value == null)
        {
            throw new InterpreterUndefinedIdentifier(token.location, context.callStack, "variable", id);
        }

        if (blockNode != null)
        {
            final Entity entity = Entities.makeEntity(value.getObject());
            blockNode.eval(context.newEntity(entity));
            value = entity.getValue();
        }

        return value;
    }
}
