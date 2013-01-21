package traci.lang.interpreter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import traci.lang.interpreter.TraciValue.Type;
import traci.lang.interpreter.exceptions.InterpreterIllegalArgumentType;
import traci.lang.interpreter.exceptions.InterpreterIllegalNumberOfArguments;
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

    private abstract static class UnaryNumericalFunction extends BuiltinFunction
    {
        private UnaryNumericalFunction(final String id)
        {
            super(id);
        }

        protected abstract double calc(final double arg);

        @Override
        public TraciValue invoke(final FunctionCallNode funcallNode, final Context context, final List<TraciValue> args)
                throws InterpreterIllegalNumberOfArguments, InterpreterIllegalArgumentType
        {
            final IncludeLocation location = funcallNode.getToken().location;

            if (args.size() != 1)
            {
                throw new InterpreterIllegalNumberOfArguments(location, context.callStack, id, 1, args.size());
            }

            final TraciValue arg = args.get(0);

            if (arg.getType() != Type.NUMBER)
            {
                throw new InterpreterIllegalArgumentType(location, context.callStack, id, Type.NUMBER, arg.getType(), 1);
            }

            return new TraciValue(calc(arg.getNumber()));
        }
    }

    private static final BuiltinFunction PRINT = new BuiltinFunction("print")
    {
        @Override
        public TraciValue invoke(final FunctionCallNode funcallNode, final Context context, final List<TraciValue> args)
                throws InterpreterIllegalNumberOfArguments
        {
            final IncludeLocation location = funcallNode.getToken().location;

            if (args.size() != 1)
            {
                throw new InterpreterIllegalNumberOfArguments(location, context.callStack, id, 1, args.size());
            }

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
