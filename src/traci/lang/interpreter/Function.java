package traci.lang.interpreter;

import java.util.List;

import traci.lang.interpreter.exceptions.InterpreterRuntimeException;

public interface Function
{
    public TraciValue invoke(Context context, List<TraciValue> args) throws InterpreterRuntimeException;

    public int numArgs();
}
