package traci.lang.interpreter.exceptions;

import traci.lang.interpreter.CallStack;
import traci.lang.interpreter.TraciValue.Type;
import traci.lang.parser.IncludeLocation;

@SuppressWarnings("serial")
public class InterpreterIllegalArgumentType extends InterpreterRuntimeException
{
    public final String function;
    public final Type expectedArgType;
    public final Type gotArgType;
    public final int argIndex;

    private static String makeMsg(final String function, final Type expectedArgType, final Type gotArgType,
            final int argIndex)
    {
        return "Incorrect type for argument " + argIndex + " of '" + function + "': expected '"
                + expectedArgType.toString() + "' got '" + gotArgType.toString() + "'";
    }

    public InterpreterIllegalArgumentType(final IncludeLocation location, final CallStack callStack,
            final String function, final Type expectedArgType, final Type gotArgType, final int argIndex)
    {
        super(location, callStack, makeMsg(function, expectedArgType, gotArgType, argIndex));

        this.function = function;
        this.expectedArgType = expectedArgType;
        this.gotArgType = gotArgType;
        this.argIndex = argIndex;
    }
}
