package se.ejp.traci.lang.interpreter.node;

import java.util.ArrayList;
import java.util.List;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.interpreter.functions.FunctionSet;

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

    public List<TraciNode> getStatements()
    {
        return statements;
    }

    public BlockNode append(final BlockNode other)
    {
        final FunctionSet combinedFunctions = this.functions.clone();
        combinedFunctions.mergeFrom(other.functions);

        final BlockNode combined = new BlockNode(combinedFunctions);

        // Add all statements from this node
        for (final TraciNode statement : this.statements)
        {
            combined.addStatement(statement);
        }

        // Add all statements from the other node
        for (final TraciNode statement : other.statements)
        {
            combined.addStatement(statement);
        }

        return combined;
    }
}
