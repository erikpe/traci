package traci.lang.interpreter.node;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
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
            final String msg = "Undefined variable '" + id + "'";
            throw new InterpreterRuntimeException(token.location, context.callStack, msg);
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
