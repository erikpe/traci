package traci.lang.parser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import traci.lang.parser.TraciLocation.FileLocation;

public class TraciCallStack
{
    private final List<FileLocation> fileLocations;
    private final List<String> functions;

    private TraciCallStack(final List<FileLocation> fileLocations, final List<String> functions)
    {
        this.fileLocations = fileLocations;
        this.functions = functions;
    }

    public TraciCallStack push(final FileLocation location, final String function)
    {
        final List<FileLocation> newFileLocations = new ArrayList<FileLocation>(fileLocations);
        final List<String> newFunctions = new ArrayList<String>(functions);

        newFileLocations.add(location);
        newFunctions.add(function);

        return new TraciCallStack(newFileLocations, newFunctions);
    }

    public static TraciCallStack empty()
    {
        return new TraciCallStack(Collections.<FileLocation>emptyList(), Collections.<String>singletonList("<root>"));
    }

    public String print(final FileLocation bottomLocation)
    {
        final StringBuilder sb = new StringBuilder();

        for (int i = functions.size() - 1; i >= 0; --i)
        {
            final FileLocation location;

            if (i == (functions.size() - 1))
            {
                location = bottomLocation;
            }
            else
            {
                location = fileLocations.get(i);
            }

            sb.append("    at ").append(functions.get(i));
            sb.append(" (").append(location.toString()).append(")");

            if (i >= 1)
            {
                sb.append("\n");
            }
        }

        return sb.toString();
    }
}