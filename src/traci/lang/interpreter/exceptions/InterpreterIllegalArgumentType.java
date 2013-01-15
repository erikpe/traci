package traci.lang.interpreter.exceptions;

import traci.lang.interpreter.CallStack;
import traci.lang.interpreter.TraciValue.Type;
import traci.lang.parser.IncludeLocation;

@SuppressWarnings("serial")
public class InterpreterIllegalArgumentType extends InterpreterRuntimeException
{
    public final String function;
    public final Type argType;
    public final Type expectedArgType;
    public final int argIndex;

    private static String makeMsg(final String function, final Type argType, final Type expectedArgType,
            final int argIndex)
    {
        return "Incorrect type for argument " + argIndex + " of '" + function + "': got " + argType.toString()
                + " expected " + expectedArgType.toString();
    }

    public InterpreterIllegalArgumentType(final IncludeLocation location, final CallStack callStack,
            final String function, final Type argType, final Type expectedArgType, final int argIndex)
    {
        super(location, callStack, makeMsg(function, argType, expectedArgType, argIndex));

        this.function = function;
        this.argType = argType;
        this.expectedArgType = expectedArgType;
        this.argIndex = argIndex;
    }
}
