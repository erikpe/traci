package se.ejp.traci.lang.parser;

import org.antlr.runtime.RecognitionException;

public class ParseError
{
    public static enum Source { LEXER, PARSER };

    public final RecognitionException e;
    public final IncludeLocation includeLocation;
    public final String msg;
    public final Source source;

    public ParseError(final RecognitionException e, final IncludeLocation includeLocation, final String msg,
            final Source source)
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
            sb.append("Parse error: ");
            break;
        }

        sb.append(msg);

        return sb.toString();
    }

    private static String[] customTokenNames = null;

    private static void initializeCustomTokenNames(final TraciParser parser)
    {
        final String[] tokenNames = parser.getTokenNames();
        customTokenNames = new String[tokenNames.length];
        System.arraycopy(tokenNames, 0, customTokenNames, 0, tokenNames.length);

        customTokenNames[TraciParser.LTE_OP]    = "'<='";
        customTokenNames[TraciParser.GTE_OP]    = "'>='";
        customTokenNames[TraciParser.EQ_OP]     = "'=='";
        customTokenNames[TraciParser.NEQ_OP]    = "'!='";
        customTokenNames[TraciParser.LT_OP]     = "'<'";
        customTokenNames[TraciParser.GT_OP]     = "'>'";
        customTokenNames[TraciParser.PLUS_OP]   = "'+'";
        customTokenNames[TraciParser.MINUS_OP]  = "'-'";
        customTokenNames[TraciParser.MUL_OP]    = "'*'";
        customTokenNames[TraciParser.DIV_OP]    = "'/'";
        customTokenNames[TraciParser.NOT_OP]    = "'!'";
        customTokenNames[TraciParser.COMMA_OP]  = "','";
        customTokenNames[TraciParser.ASSIGN]    = "'='";
        customTokenNames[TraciParser.SEMICOLON] = "';'";
        customTokenNames[TraciParser.LBRACKET]  = "'['";
        customTokenNames[TraciParser.RBRACKET]  = "']'";
        customTokenNames[TraciParser.LPAR]      = "'('";
        customTokenNames[TraciParser.RPAR]      = "')'";
        customTokenNames[TraciParser.LCURLY]    = "'{'";
        customTokenNames[TraciParser.RCURLY]    = "'}'";
        customTokenNames[TraciParser.DOTS]      = "'..'";

        customTokenNames[TraciParser.DEF]       = "'def'";
        customTokenNames[TraciParser.RETURN]    = "'return'";
        customTokenNames[TraciParser.GLOBAL]    = "'global'";
        customTokenNames[TraciParser.WHILE]     = "'while'";
        customTokenNames[TraciParser.IF]        = "'if'";
        customTokenNames[TraciParser.ELSE]      = "'else'";
        customTokenNames[TraciParser.BBOX]      = "'bbox'";
        customTokenNames[TraciParser.FOR]       = "'for'";
        customTokenNames[TraciParser.IN]        = "'in'";
    }

    static String[] getCustomTokenNames(final TraciParser parser)
    {
        if (customTokenNames == null)
        {
            initializeCustomTokenNames(parser);
        }

        return customTokenNames;
    }
}
