package se.ejp.traci.lang.interpreter.node;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.TraciValue;

public class ConstNode implements TraciNode
{
    public final TraciValue value;

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
