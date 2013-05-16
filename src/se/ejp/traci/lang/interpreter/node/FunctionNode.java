package se.ejp.traci.lang.interpreter.node;

import java.util.List;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalNumberOfArguments;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.interpreter.functions.Function;
import se.ejp.traci.lang.parser.IncludeLocation;

public class FunctionNode implements Function
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

    @Override
    public String toString()
    {
        return id + "()";
    }
}
