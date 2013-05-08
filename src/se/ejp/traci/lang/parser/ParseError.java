package se.ejp.traci.lang.parser;

import org.antlr.runtime.RecognitionException;

public class ParseError
{
    public final RecognitionException e;
    public final IncludeLocation includeLocation;
    public final String msg;

    public ParseError(final RecognitionException e, final IncludeLocation includeLocation, final String msg)
    {
        this.e = e;
        this.includeLocation = includeLocation;
        this.msg = msg;
    }
}
