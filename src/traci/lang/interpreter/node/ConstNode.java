package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.TraciValue;

public class ConstNode implements TraciNode
{
    private final TraciValue value;

    public ConstNode(final TraciValue value)
    {
        this.value = value;
    }

    @Override
    public TraciValue eval(final Context context)
    {
        return value;
    }
}
