package traci.lang.interpreter.node;

import java.util.List;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Function;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterIllegalNumberOfArguments;
import traci.lang.interpreter.exceptions.InterpreterInternalException;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.parser.IncludeLocation;

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
    public TraciValue invoke(final FunctionCallNode funcallNode, Context context, final List<TraciValue> args)
            throws InterpreterRuntimeException
    {
        final IncludeLocation location = funcallNode.getToken().location;

        if (args.size() != argIDs.size())
        {
            throw new InterpreterIllegalNumberOfArguments(location, context.callStack, id, argIDs.size(), args.size());
        }

        context = context.newFuncallContext(funcallNode.getToken().location.fileLocation, id);

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

        return returnValue;
    }
}
