package se.ejp.traci.lang.interpreter.functions;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class FunctionSet
{
    private final FunctionSet outerScope;
    private final Map<String, Function> functions;

    public FunctionSet(final FunctionSet outerScope)
    {
        this.outerScope = outerScope;
        this.functions = new HashMap<String, Function>();
    }

    public Function get(final String id)
    {
        final Function function = functions.get(id);

        if (function == null && outerScope != null)
        {
            return outerScope.get(id);
        }

        return function;
    }

    public void put(final String id, final Function function)
    {
        functions.put(id, function);
    }

    private Set<String> getAllFunctionIds()
    {
        final Set<String> allFunctionIds;
        if (outerScope != null)
        {
            allFunctionIds = outerScope.getAllFunctionIds();
        }
        else
        {
            allFunctionIds = new HashSet<String>();
        }

        allFunctionIds.addAll(functions.keySet());

        return allFunctionIds;
    }

    @Override
    public String toString()
    {
        return getAllFunctionIds().toString();
    }
}
