package traci.lang.interpreter.node;

import java.util.List;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
import traci.lang.interpreter.Function;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterInternalException;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;

public class FunctionNode implements TraciNode, Function
{
    public final String id;
    public final List<String> argIDs;
    private final BlockNode bodyNode;

    public FunctionNode(final String id, final List<String> argIDs, final BlockNode bodyNode)
    {
        this.id = id;
        this.argIDs = argIDs;
        this.bodyNode = bodyNode;
    }

    @Override
    public TraciValue eval(final Context dummy)
    {
        throw new InterpreterInternalException("FunctionNode.eval() should never be called.");
    }

    @Override
    public TraciValue invoke(final FunctionCallNode funcallNode, final Context context, final List<TraciValue> args)
            throws InterpreterRuntimeException
    {
        assert argIDs.size() == args.size();

        context.pushEntity(Entities.NULL_ENTITY);

        for (int i = 0; i < argIDs.size(); ++i)
        {
            context.putLocalValue(argIDs.get(i), args.get(i));
        }

        TraciValue returnValue = null;
        try
        {
            bodyNode.eval(context);
        }
        catch (final FunctionReturnException e)
        {
            returnValue = e.value;
        }

        context.popEntity();

        return returnValue;
    }

    @Override
    public int numArgs()
    {
        return argIDs.size();
    }
}
