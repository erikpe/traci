package traci.lang.grammar;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonToken;

@SuppressWarnings("serial")
public class TraciToken extends CommonToken
{
    public final String filename;
    public final List<Location> includePath;

    static String unquote(String str)
    {
        if (str == null)
        {
            return null;
        }
        str = str.substring(1, str.length() - 1);
        str = str.replace("\\\\", "\\");
        return str;
    }

    public TraciToken(final CharStream input, final int type, final int channel, final int start, final int stop,
            final String filename, final List<Location> includeStack)
    {
        super(input, type, channel, start, stop);
        this.filename = unquote(filename);
        this.includePath = Collections.unmodifiableList(new ArrayList<Location>(includeStack));
    }
}
