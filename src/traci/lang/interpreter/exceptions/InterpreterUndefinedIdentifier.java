package traci.lang.interpreter.exceptions;

import traci.lang.interpreter.CallStack;
import traci.lang.parser.IncludeLocation;

@SuppressWarnings("serial")
public class InterpreterUndefinedIdentifier extends InterpreterRuntimeException
{
    public final String type;
    public final String id;

    private static String makeMsg(final String type, final String id)
    {
        return "Undefined " + type + " '" + id + "'";
    }

    public InterpreterUndefinedIdentifier(final IncludeLocation location, final CallStack callStack, final String type,
            final String id)
    {
        super(location, callStack, makeMsg(type, id));

        this.type = type;
        this.id = id;
    }
}
