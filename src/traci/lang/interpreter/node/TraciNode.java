package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.InterpreterRuntimeException;
import traci.lang.interpreter.TraciValue;

public interface TraciNode
{
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException;
}
