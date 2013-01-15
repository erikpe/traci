package traci.lang.interpreter.exceptions;

import traci.lang.interpreter.CallStack;
import traci.lang.parser.IncludeLocation;

@SuppressWarnings("serial")
public class InterpreterIllegalNumberOfArguments extends InterpreterRuntimeException
{
    public final String function;
    public final int numArgs;
    public final int expectedNumArgs;

    private static String makeMsg(final String function, final int numArgs, final int expectedNumArgs)
    {
        assert numArgs != expectedNumArgs;

        final String amount;
        if (numArgs < expectedNumArgs)
        {
            amount = "few";
        }
        else
        {
            amount = "many";
        }

        return "Too " + amount + " arguments for '" + function + "': got " + numArgs + " arguments, expected "
                + expectedNumArgs;
    }

    public InterpreterIllegalNumberOfArguments(final IncludeLocation location, final CallStack callStack, final String function, final int numArgs, final int expectedNumArgs)
    {
        super(location, callStack, makeMsg(function, numArgs, expectedNumArgs));

        this.function = function;
        this.numArgs = numArgs;
        this.expectedNumArgs = expectedNumArgs;
    }
}
