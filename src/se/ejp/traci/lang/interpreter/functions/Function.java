package se.ejp.traci.lang.interpreter.functions;

import java.util.List;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.interpreter.node.FunctionCallNode;

public interface Function
{
    public TraciValue invoke(final FunctionCallNode funcallNode, final Context context, final List<TraciValue> args)
            throws InterpreterRuntimeException;
}
