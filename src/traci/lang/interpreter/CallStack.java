package traci.lang.interpreter;

import java.util.Stack;

import traci.lang.parser.IncludeLocation.FileLocation;

public class CallStack
{
    private final Stack<FileLocation> fileLocations;
    private final Stack<String> functions;
    private String currentFunction;

    public CallStack()
    {
        this.fileLocations = new Stack<FileLocation>();
        this.functions = new Stack<String>();
        this.currentFunction = "<root>";
    }

    public void push(final FileLocation location, final String function)
    {
        fileLocations.push(location);
        functions.push(currentFunction);
        currentFunction = function;
    }

    public void pop()
    {
        fileLocations.pop();
        currentFunction = functions.pop();
    }

    public String print(final FileLocation currentLocation)
    {
        final StringBuilder sb = new StringBuilder();

        push(currentLocation, "DUMMY-FUNCTION");
        for (int i = functions.size() - 1; i >= 0; --i)
        {
            sb.append("    at ").append(functions.get(i));
            sb.append(" (").append(fileLocations.get(i).toString()).append(")");

            if (i >= 1)
            {
                sb.append("\n");
            }
        }
        pop();

        return sb.toString();
    }
}
