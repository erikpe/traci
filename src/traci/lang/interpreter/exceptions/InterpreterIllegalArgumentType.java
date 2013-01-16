package traci.lang.interpreter.exceptions;

import java.util.Collections;
import java.util.Set;

import traci.lang.interpreter.CallStack;
import traci.lang.interpreter.TraciValue.Type;
import traci.lang.parser.IncludeLocation;

@SuppressWarnings("serial")
public class InterpreterIllegalArgumentType extends InterpreterRuntimeException
{
    public final String function;
    public final Set<Type> expectedArgType;
    public final Type gotArgType;
    public final int argIndex;

    private static String makeMsg(final String function, final Set<Type> expectedArgType, final Type gotArgType,
            final int argIndex)
    {
        final StringBuilder sb = new StringBuilder();
        final Type[] et = expectedArgType.toArray(new Type[expectedArgType.size()]);

        sb.append("Incorrect type for argument ").append(argIndex);
        sb.append(" of '").append(function).append("': expected '");

        for (int i = 0; i < et.length; ++i)
        {
            sb.append(et[i].toString());
            if (i < et.length - 1)
            {
                sb.append(" or ");
            }
        }

        sb.append("' got '").append(gotArgType.toString()).append("'");
        return sb.toString();
    }

    public InterpreterIllegalArgumentType(final IncludeLocation location, final CallStack callStack,
            final String function, final Set<Type> expectedArgType, final Type gotArgType, final int argIndex)
    {
        super(location, callStack, makeMsg(function, expectedArgType, gotArgType, argIndex));

        assert !expectedArgType.isEmpty();

        this.function = function;
        this.expectedArgType = expectedArgType;
        this.gotArgType = gotArgType;
        this.argIndex = argIndex;
    }

    public InterpreterIllegalArgumentType(final IncludeLocation location, final CallStack callStack,
            final String function, final Type expectedArgType, final Type gotArgType, final int argIndex)
    {
        this(location, callStack, function, Collections.<Type>singleton(expectedArgType), gotArgType, argIndex);
    }
}
