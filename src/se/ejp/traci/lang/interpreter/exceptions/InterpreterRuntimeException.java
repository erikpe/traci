package se.ejp.traci.lang.interpreter.exceptions;

import se.ejp.traci.lang.interpreter.CallStack;
import se.ejp.traci.lang.parser.IncludeLocation;

@SuppressWarnings("serial")
public class InterpreterRuntimeException extends Exception
{
    private IncludeLocation includeLocation;
    private CallStack callStack;
    private final String msg;

    public InterpreterRuntimeException(final IncludeLocation includeLocation, final CallStack callStack, final String msg)
    {
        this.includeLocation = includeLocation;
        this.callStack = callStack;
        this.msg = msg;
    }

    public void setLocation(final IncludeLocation includeLocation)
    {
        this.includeLocation = includeLocation;
    }

    public void setCallStack(final CallStack callStack)
    {
        this.callStack = callStack;
    }

    public IncludeLocation getLocation()
    {
        return includeLocation;
    }

    public CallStack getCallStack()
    {
        return callStack;
    }

    public String fullMsg()
    {
        final StringBuilder sb = new StringBuilder();

        if (includeLocation != null)
        {
            includeLocation.toString(sb);
            sb.append('\n');
        }

        if (callStack != null)
        {
            callStack.format(sb, includeLocation.fileLocation);
            sb.append('\n');
        }

        sb.append("Runtime error: ").append(msg);

        return sb.toString();
    }
}
