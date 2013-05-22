package se.ejp.traci.lang.parser;

import org.antlr.runtime.RecognitionException;

class ParserUtilities
{
    static String unquoteQstring(String str)
    {
        if (str == null)
        {
            return null;
        }

        str = str.substring(1, str.length() - 1);
        str = str.replace("\\\\", "\\");

        return str;
    }

    public static String makeErrorMessage(final RecognitionException e, final String source)
    {
        final StringBuilder sb = new StringBuilder();

        if (e.token != null)
        {
            final IncludeLocation location = ((TraciToken) e.token).location;
            location.toString(sb);
            sb.append('\n');
        }

        sb.append(source).append(": ").append(e.getMessage());

        return sb.toString();
    }
}
