package se.ejp.traci.lang.interpreter.exceptions;

import java.io.PrintWriter;
import java.io.StringWriter;

@SuppressWarnings("serial")
public class InterpreterInternalException extends RuntimeException
{
    public InterpreterInternalException(final String msg)
    {
        super(msg);
    }

    public InterpreterInternalException(final Throwable e)
    {
        super(e);
    }

    public String fullMsg()
    {
        final StringWriter writer = new StringWriter();
        printStackTrace(new PrintWriter(writer));
        return writer.toString();
    }
}
