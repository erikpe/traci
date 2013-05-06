package se.ejp.traci.lang.interpreter.functions;

import java.util.List;
import java.util.Random;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalArgumentType;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalNumberOfArguments;
import se.ejp.traci.lang.interpreter.node.FunctionCallNode;
import se.ejp.traci.lang.parser.IncludeLocation;

public class BuiltinFunctions
{

    private abstract static class BuiltinFunction implements Function
    {
        protected final String id;

        private BuiltinFunction(final String id)
        {
            this.id = id;
        }

        @Override
        public String toString()
        {
            return id + "()";
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

    private static final BuiltinFunction RAND = new BuiltinFunction("rand")
    {
        private final Random random = new Random(0);

        @Override
        public final TraciValue invoke(final FunctionCallNode funcallNode, final Context context, final List<TraciValue> args)
                throws InterpreterIllegalNumberOfArguments
        {
            final IncludeLocation location = funcallNode.getToken().location;

            if (args.size() != 0)
            {
                throw new InterpreterIllegalNumberOfArguments(location, context.callStack, id, 1, args.size());
            }

            return new TraciValue(random.nextDouble());
        }
    };

    private static final BuiltinFunction RANDINT = new BuiltinFunction("randint")
    {
        private final Random random = new Random(0);

        @Override
        public final TraciValue invoke(final FunctionCallNode funcallNode, final Context context, final List<TraciValue> args)
                throws InterpreterIllegalNumberOfArguments, InterpreterIllegalArgumentType
        {
            final IncludeLocation location = funcallNode.getToken().location;

            if (args.size() != 2)
            {
                throw new InterpreterIllegalNumberOfArguments(location, context.callStack, id, 1, args.size());
            }

            if (args.get(0).getType() != Type.NUMBER)
            {
                throw new InterpreterIllegalArgumentType(location, context.callStack, id, Type.NUMBER, args.get(0)
                        .getType(), 1);
            }

            if (args.get(1).getType() != Type.NUMBER)
            {
                throw new InterpreterIllegalArgumentType(location, context.callStack, id, Type.NUMBER, args.get(1)
                        .getType(), 1);
            }

            final int start = args.get(0).getNumber().intValue();
            final int end = args.get(1).getNumber().intValue();
            final int value = random.nextInt(end - start + 1) + start;

            return new TraciValue(Double.valueOf(value));
        }
    };

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

    private static final BuiltinFunction SQRT = new UnaryNumericalFunction("sqrt")
    {
        @Override
        protected double calc(final double arg)
        {
            return Math.sqrt(arg);
        }
    };

    private static final BuiltinFunction LENGTH = new BuiltinFunction("length")
    {
        @Override
        public TraciValue invoke(final FunctionCallNode funcallNode, final Context context, final List<TraciValue> args)
                throws InterpreterIllegalNumberOfArguments, InterpreterIllegalArgumentType
        {
            final IncludeLocation location = funcallNode.getToken().location;

            if (args.size() != 1)
            {
                throw new InterpreterIllegalNumberOfArguments(location, context.callStack, id, 1, args.size());
            }

            if (args.get(0).getType() != Type.VECTOR)
            {
                throw new InterpreterIllegalArgumentType(location, context.callStack, id, Type.VECTOR, args.get(0)
                        .getType(), 1);
            }

            final Double length = args.get(0).getVector().length();

            return new TraciValue(length);
        }
    };

    private static final BuiltinFunction[] ALL_BUILTIN_FUNCTIONS = new BuiltinFunction[] { PRINT, SIN, COS, RAND,
            RANDINT, SQRT, LENGTH };

    public static FunctionSet getAll()
    {
        final FunctionSet functionSet = new FunctionSet(null);

        for (final BuiltinFunction function : ALL_BUILTIN_FUNCTIONS)
        {
            functionSet.put(function.id, function);
        }

        return functionSet;
    }
}
