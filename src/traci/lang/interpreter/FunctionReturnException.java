package traci.lang.interpreter;

@SuppressWarnings("serial")
public class FunctionReturnException extends Exception
{
    public final TraciValue value;

    public FunctionReturnException(final TraciValue value)
    {
        this.value = value;
    }
}
