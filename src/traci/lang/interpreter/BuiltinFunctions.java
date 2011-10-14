package traci.lang.interpreter;

import java.util.List;

public class BuiltinFunctions
{
    public static final Function FUNCTION_PRINT = new Function()
    {
        public TraciValue invoke(final Context context, final List<TraciValue> args)
        {
            System.out.println(args.get(0).toString());
            return null;
        }
    };
}
