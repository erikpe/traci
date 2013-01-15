package traci.lang.interpreter.exceptions;

import traci.lang.interpreter.TraciValue;

@SuppressWarnings("serial")
public class FunctionReturnException extends Exception
{
    public final TraciValue value;

    public FunctionReturnException(final TraciValue value)
    {
        this.value = value;
    }
}
