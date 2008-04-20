package traci.parser;

public class ParseError extends Exception
{
    private static final long serialVersionUID = -9096027031728222902L;
    
    public ParseError(final String str)
    {
        super(str);
    }
}
