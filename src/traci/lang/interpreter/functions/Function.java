package traci.lang.interpreter.functions;

import java.util.List;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.interpreter.node.FunctionCallNode;

public interface Function
{
    public TraciValue invoke(final FunctionCallNode funcallNode, final Context context, final List<TraciValue> args)
            throws InterpreterRuntimeException;
}
