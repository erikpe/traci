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

    private abstract static class TypeCheckedBuiltinFunction extends BuiltinFunction
    {
        private final Type[] argTypes;

        private TypeCheckedBuiltinFunction(final String id, final Type ... argTypes)
        {
            super(id);
            this.argTypes = argTypes;
        }

        protected abstract TraciValue invoke(final List<TraciValue> args);

        @Override
        public TraciValue invoke(final FunctionCallNode funcallNode, final Context context, final List<TraciValue> args)
                throws InterpreterIllegalNumberOfArguments, InterpreterIllegalArgumentType
        {
            final IncludeLocation location = funcallNode.getToken().location;

            if (args.size() != argTypes.length)
            {
                throw new InterpreterIllegalNumberOfArguments(location, context.callStack, id, argTypes.length, args.size());
            }

            for (int i = 0; i < args.size(); ++i)
            {
                if (argTypes[i] != null && args.get(i).getType() != argTypes[i])
                {
                    throw new InterpreterIllegalArgumentType(location, context.callStack, id, argTypes[i],
                            args.get(i).getType(), i + 1);
                }
            }

            return invoke(args);
        }
    }

    private abstract static class UnaryNumericalFunction extends TypeCheckedBuiltinFunction
    {
        private UnaryNumericalFunction(final String id)
        {
            super(id, Type.NUMBER);
        }

        protected abstract double calc(final double arg);

        @Override
        public TraciValue invoke(final List<TraciValue> args)
        {
            return new TraciValue(calc(args.get(0).getNumber()));
        }
    }

    private static final BuiltinFunction RAND = new TypeCheckedBuiltinFunction("rand")
    {
        private final Random random = new Random(0);

        @Override
        public final TraciValue invoke(final List<TraciValue> args)
        {
            return new TraciValue(random.nextDouble());
        }
    };

    private static final BuiltinFunction RANDINT = new TypeCheckedBuiltinFunction("randint", Type.NUMBER, Type.NUMBER)
    {
        private final Random random = new Random(0);

        @Override
        public final TraciValue invoke(final List<TraciValue> args)
        {
            final int start = args.get(0).getNumber().intValue();
            final int end = args.get(1).getNumber().intValue();
            final int value = random.nextInt(end - start + 1) + start;

            return new TraciValue(Double.valueOf(value));
        }
    };

    private static final BuiltinFunction PRINT = new TypeCheckedBuiltinFunction("print", (Type) null)
    {
        @Override
        public TraciValue invoke(final List<TraciValue> args)
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

    private static final BuiltinFunction SQRT = new UnaryNumericalFunction("sqrt")
    {
        @Override
        protected double calc(final double arg)
        {
            return Math.sqrt(arg);
        }
    };

    private static final BuiltinFunction LENGTH = new TypeCheckedBuiltinFunction("length", Type.VECTOR)
    {
        @Override
        public TraciValue invoke(final List<TraciValue> args)
        {
            return new TraciValue(args.get(0).getVector().length());
        }
    };

    private static final BuiltinFunction DOT_PRODUCT = new TypeCheckedBuiltinFunction("dot", Type.VECTOR, Type.VECTOR)
    {
        @Override
        public TraciValue invoke(final List<TraciValue> args)
        {
            return new TraciValue(args.get(0).getVector().dot(args.get(1).getVector()));
        }
    };

    private static final BuiltinFunction CROSS_PRODUCT = new TypeCheckedBuiltinFunction("cross", Type.VECTOR, Type.VECTOR)
    {
        @Override
        public TraciValue invoke(final List<TraciValue> args)
        {
            return new TraciValue(args.get(0).getVector().cross(args.get(1).getVector()));
        }
    };

    private static final BuiltinFunction[] ALL_BUILTIN_FUNCTIONS = new BuiltinFunction[] { PRINT, SIN, COS, RAND,
            RANDINT, SQRT, LENGTH, DOT_PRODUCT, CROSS_PRODUCT };

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
