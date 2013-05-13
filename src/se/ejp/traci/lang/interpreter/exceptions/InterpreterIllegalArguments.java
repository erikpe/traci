package se.ejp.traci.lang.interpreter.exceptions;

import java.util.List;

import se.ejp.traci.lang.interpreter.CallStack;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.parser.IncludeLocation;

@SuppressWarnings("serial")
public class InterpreterIllegalArguments extends InterpreterRuntimeException
{
    public final String function;
    public final List<Type> gotArgTypes;

    private static String makeMsg(final String function, final List<Type> gotArgTypes)
    {
        final StringBuilder sb = new StringBuilder();

        sb.append("Unable to call '").append(function);
        sb.append("' with arguments: (");

        for (int i = 0; i < gotArgTypes.size(); ++i)
        {
            sb.append(gotArgTypes.get(i).toString());
            if (i < gotArgTypes.size() - 1)
            {
                sb.append(", ");
            }
        }
        sb.append(')');

        return sb.toString();
    }

    public InterpreterIllegalArguments(final IncludeLocation location, final CallStack callStack, final String function,
            final List<Type> gotArgTypes)
    {
        super(location, callStack, makeMsg(function, gotArgTypes));

        this.function = function;
        this.gotArgTypes = gotArgTypes;
    }
}
