package se.ejp.traci.lang.parser;

import org.antlr.runtime.RecognitionException;

public class ParseError
{
    public static enum Source { LEXER, PARSER };

    public final RecognitionException e;
    public final IncludeLocation includeLocation;
    public final String msg;
    public final Source source;

    public ParseError(final RecognitionException e, final IncludeLocation includeLocation, final String msg, final Source source)
    {
        this.e = e;
        this.includeLocation = includeLocation;
        this.msg = msg;
        this.source = source;
    }

    public String fullMsg()
    {
        final StringBuilder sb = new StringBuilder();

        if (includeLocation != null)
        {
            includeLocation.toString(sb);
            sb.append('\n');
        }

        switch (source)
        {
        case LEXER:
            sb.append("Lexer error: ");
            break;

        case PARSER:
            sb.append("Parser error: ");
            break;
        }

        sb.append(msg);

        return sb.toString();
    }
}
