package traci.lang.interpreter.node;

import java.util.ArrayList;
import java.util.List;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.interpreter.functions.FunctionSet;

public class BlockNode implements TraciNode
{
    private final List<TraciNode> statements;
    private final FunctionSet functions;

    public BlockNode(final FunctionSet functions)
    {
        this.statements = new ArrayList<TraciNode>();
        this.functions = functions;
    }

    @Override
    public TraciValue eval(Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        context = context.newFunctions(functions);

        for (final TraciNode statement : statements)
        {
            final TraciValue value = statement.eval(context);

            if (value != null)
            {
                context.applyValue(value);
            }
        }

        return null;
    }

    public void addStatement(final TraciNode statement)
    {
        assert(statement != null);
        statements.add(statement);
    }
}
