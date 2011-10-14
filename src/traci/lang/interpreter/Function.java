package traci.lang.interpreter;

import java.util.List;

public interface Function
{
    public TraciValue invoke(Context context, List<TraciValue> args);
}
