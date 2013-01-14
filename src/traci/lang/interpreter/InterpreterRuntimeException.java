package traci.lang.interpreter;

import traci.lang.parser.IncludeLocation;

@SuppressWarnings("serial")
public class InterpreterRuntimeException extends Exception
{
    public final IncludeLocation includeLocation;
    public final String msg;
    public final CallStack callStack;

    public InterpreterRuntimeException(final IncludeLocation includeLocation, final String msg, final CallStack callStack)
    {
        this.includeLocation = includeLocation;
        this.msg = msg;
        this.callStack = callStack;
    }
}
