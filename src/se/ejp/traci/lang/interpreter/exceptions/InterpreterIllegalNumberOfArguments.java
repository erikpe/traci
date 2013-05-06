package se.ejp.traci.lang.interpreter.exceptions;

import se.ejp.traci.lang.interpreter.CallStack;
import se.ejp.traci.lang.parser.IncludeLocation;

@SuppressWarnings("serial")
public class InterpreterIllegalNumberOfArguments extends InterpreterRuntimeException
{
    public final String function;
    public final int expectedNumArgs;
    public final int gotNumArgs;

    private static String makeMsg(final String function, final int expectedNumArgs, final int gotNumArgs)
    {
        assert gotNumArgs != expectedNumArgs;

        final String amount;
        if (gotNumArgs < expectedNumArgs)
        {
            amount = "few";
        }
        else
        {
            amount = "many";
        }

        return "Too " + amount + " arguments for '" + function + "': expected " + expectedNumArgs + " arguments, got "
                + gotNumArgs;
    }

    public InterpreterIllegalNumberOfArguments(final IncludeLocation location, final CallStack callStack,
            final String function, final int expectedNumArgs, final int gotNumArgs)
    {
        super(location, callStack, makeMsg(function, expectedNumArgs, gotNumArgs));

        this.function = function;
        this.expectedNumArgs = expectedNumArgs;
        this.gotNumArgs = gotNumArgs;
    }
}
