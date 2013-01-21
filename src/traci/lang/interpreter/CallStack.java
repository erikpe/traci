package traci.lang.interpreter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import traci.lang.parser.IncludeLocation.FileLocation;

public class CallStack
{
    private final List<FileLocation> fileLocations;
    private final List<String> functions;
    private final String currentFunction;

    private CallStack(final List<FileLocation> fileLocations, final List<String> functions, final String currentFunction)
    {
        this.fileLocations = fileLocations;
        this.functions = functions;
        this.currentFunction = currentFunction;
    }

    public CallStack push(final FileLocation location, final String function)
    {
        final List<FileLocation> newFileLocations = new ArrayList<FileLocation>(fileLocations);
        final List<String> newFunctions = new ArrayList<String>(functions);

        newFileLocations.add(location);
        newFunctions.add(currentFunction);

        return new CallStack(newFileLocations, newFunctions, function);
    }

    public static CallStack makeEmpty()
    {
        return new CallStack(Collections.<FileLocation>emptyList(), Collections.<String>emptyList(), "<root>");
    }

    public String format(final FileLocation currentLocation)
    {
        final StringBuilder sb = new StringBuilder();

        if (currentLocation != null)
        {
            sb.append("    at ").append(currentFunction);
            sb.append(" (").append(currentLocation.toString()).append(')');
        }

        for (int i = functions.size() - 1; i >= 0; --i)
        {
            if (currentLocation != null || i < functions.size() - 1)
            {
                sb.append('\n');
            }

            sb.append("    at ").append(functions.get(i));
            sb.append(" (").append(fileLocations.get(i).toString()).append(')');
        }

        return sb.toString();
    }
}
