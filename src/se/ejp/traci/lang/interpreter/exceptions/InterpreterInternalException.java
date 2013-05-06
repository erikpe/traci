package se.ejp.traci.lang.interpreter.exceptions;

@SuppressWarnings("serial")
public class InterpreterInternalException extends RuntimeException
{
    public InterpreterInternalException(final String msg)
    {
        super(msg);
    }
}
