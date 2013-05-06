package se.ejp.traci.lang.parser;

public class ParseError
{
    public final IncludeLocation includeLocation;
    public final String msg;

    public ParseError(final IncludeLocation includeLocation, final String msg)
    {
        this.includeLocation = includeLocation;
        this.msg = msg;
    }
}
