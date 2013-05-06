package se.ejp.traci.lang.interpreter.node;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;

public interface TraciNode
{
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException;
}
