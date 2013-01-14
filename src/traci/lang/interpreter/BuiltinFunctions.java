package traci.lang.interpreter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class BuiltinFunctions
{
    private abstract static class BuiltinFunction implements Function
    {
        private final String id;

        private BuiltinFunction(final String id)
        {
            this.id = id;
        }

        @Override
        public int numArgs()
        {
            return 1;
        }
    }

    private static final BuiltinFunction PRINT = new BuiltinFunction("print")
    {
        @Override
        public TraciValue invoke(final Context context, final List<TraciValue> args)
        {
            System.out.println(args.get(0).toString());
            return null;
        }
    };

    private static final BuiltinFunction SIN = new BuiltinFunction("sin")
    {
        @Override
        public TraciValue invoke(final Context context, final List<TraciValue> args)
        {
            final Double arg = args.get(0).getNumber();
            return new TraciValue(Double.valueOf(Math.sin(arg)));
        }
    };

    private static final BuiltinFunction COS = new BuiltinFunction("cos")
    {
        @Override
        public TraciValue invoke(final Context context, final List<TraciValue> args)
        {
            final Double arg = args.get(0).getNumber();
            return new TraciValue(Double.valueOf(Math.sin(arg)));
        }
    };

    public static Map<String, Function> getAll()
    {
        final Map<String, Function> functions = new HashMap<String, Function>();

        functions.put(PRINT.id, PRINT);
        functions.put(SIN.id, SIN);
        functions.put(COS.id, COS);

        return functions;
    }
}
