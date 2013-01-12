package traci.lang.parser;

import java.util.List;

import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonToken;

import traci.lang.parser.TraciLocation.FileLocation;

@SuppressWarnings("serial")
public class TraciToken extends CommonToken
{
    public final TraciLocation location;

    public TraciToken(final CharStream input, final int type, final int channel, final int start, final int stop,
            final FileLocation fileLocation, final List<FileLocation> includeStack)
    {
        super(input, type, channel, start, stop);
        this.location = new TraciLocation(fileLocation, includeStack);
    }
}
