package traci.lang.interpreter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import traci.lang.interpreter.TraciValue.Type;
import traci.lang.interpreter.exceptions.InterpreterIllegalArgumentType;
import traci.lang.interpreter.node.FunctionCallNode;
import traci.lang.parser.IncludeLocation;

public class BuiltinFunctions
{
    private abstract static class BuiltinFunction implements Function
    {
        protected final String id;

        private BuiltinFunction(final String id)
        {
            this.id = id;
        }
    }

    private abstract static class UnaryBuiltinFunction extends BuiltinFunction
    {
        private UnaryBuiltinFunction(final String id)
        {
            super(id);
        }

        @Override
        public int numArgs()
        {
            return 1;
        }
    }

    private abstract static class UnaryNumericalFunction extends UnaryBuiltinFunction
    {
        private UnaryNumericalFunction(final String id)
        {
            super(id);
        }

        protected abstract double calc(final double arg);

        @Override
        public TraciValue invoke(final FunctionCallNode funcallNode, final Context context, final List<TraciValue> args)
                throws InterpreterIllegalArgumentType
        {
            assert args.size() == 1;

            final TraciValue arg = args.get(0);

            if (arg.getType() != Type.NUMBER)
            {
                IncludeLocation location = null;
                if (funcallNode != null)
                {
                    location = funcallNode.getToken().location;
                }

                throw new InterpreterIllegalArgumentType(location, context.callStack, id, Type.NUMBER,
                        arg.getType(), 1);
            }

            final double value = calc(arg.getNumber());

            return new TraciValue(value);
        }
    }

    private static final BuiltinFunction PRINT = new UnaryBuiltinFunction("print")
    {
        @Override
        public TraciValue invoke(final FunctionCallNode functionCallNode, final Context context,
                final List<TraciValue> args)
        {
            System.out.println(args.get(0).toString());
            return null;
        }
    };

    private static final BuiltinFunction SIN = new UnaryNumericalFunction("sin")
    {
        @Override
        protected double calc(final double arg)
        {
            return Math.sin(arg);
        }
    };

    private static final BuiltinFunction COS = new UnaryNumericalFunction("cos")
    {
        @Override
        protected double calc(final double arg)
        {
            return Math.cos(arg);
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
