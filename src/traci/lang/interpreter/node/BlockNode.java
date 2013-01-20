package traci.lang.interpreter.node;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Function;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;

public class BlockNode implements TraciNode
{
    private final List<TraciNode> statements;
    private final Map<String, Function> functions;

    public BlockNode(final Map<String, Function> functions)
    {
        this.statements = new ArrayList<TraciNode>();
        this.functions = functions;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        context.pushFunctions(functions);

        try
        {
            for (final TraciNode statement : statements)
            {
                final TraciValue value = statement.eval(context);

                if (value != null)
                {
                    context.applyValue(value);
                }
            }
        }
        catch (final FunctionReturnException e)
        {
            throw e;
        }
        finally
        {
            context.popFunctions();
        }

        return null;
    }

    public void addStatement(final TraciNode statement)
    {
        assert(statement != null);
        statements.add(statement);
    }
}
