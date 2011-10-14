package traci.lang.interpreter.node;

import java.util.ArrayList;
import java.util.List;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.Functions;
import traci.lang.interpreter.TraciValue;

public class BlockNode implements TraciNode
{
    private final List<TraciNode> statements;
    private final Functions functions;
    
    public BlockNode(final Functions functions)
    {
        this.statements = new ArrayList<TraciNode>();
        this.functions = functions;
    }
    
    @Override
    public TraciValue eval(Context context) throws FunctionReturnException
    {
        context = context.newFunctions(functions);
        
        for (final TraciNode statement : statements)
        {
            final TraciValue value = statement.eval(context);
            
            if (value != null)
            {
                context.entity.applyValue(value);
            }
        }
        
        return null;
    }
    
    public void addStatement(final TraciNode statement)
    {
        statements.add(statement);
    }
}
