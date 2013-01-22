package traci.lang.interpreter.functions;

import java.util.HashMap;

@SuppressWarnings("serial")
public class FunctionSet extends HashMap<String, Function>
{
    public FunctionSet()
    {
        super();
    }

    public FunctionSet(final FunctionSet other)
    {
        super(other);
    }

    @Override
    public String toString()
    {
        return keySet().toString();
    }
}
