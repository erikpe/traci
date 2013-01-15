package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;

public interface TraciNode
{
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException;
}
