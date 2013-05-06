package se.ejp.traci.lang.interpreter.exceptions;

import se.ejp.traci.lang.interpreter.CallStack;
import se.ejp.traci.lang.parser.IncludeLocation;

@SuppressWarnings("serial")
public abstract class InterpreterRuntimeException extends Exception
{
    public final IncludeLocation includeLocation;
    public final CallStack callStack;
    public final String msg;

    public InterpreterRuntimeException(final IncludeLocation includeLocation, final CallStack callStack, final String msg)
    {
        this.includeLocation = includeLocation;
        this.callStack = callStack;
        this.msg = msg;
    }
}