package se.ejp.traci.lang.interpreter.exceptions;

import java.io.IOException;

import se.ejp.traci.lang.interpreter.CallStack;
import se.ejp.traci.lang.parser.IncludeLocation;

@SuppressWarnings("serial")
public class InterpreterIOException extends InterpreterRuntimeException
{
    public InterpreterIOException(final IncludeLocation location, final CallStack callStack, final String msg,
            final IOException cause)
    {
        super(location, callStack, msg);
        initCause(cause);
    }
}
