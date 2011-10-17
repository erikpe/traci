package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;

public class AssignNode implements TraciNode
{
    private final String id;
    private final TraciNode exprNode;
    private final boolean isGlobal;
    
    public AssignNode(final String id, final TraciNode exprNode, final boolean isGlobal)
    {
        this.id = id;
        this.exprNode = exprNode;
        this.isGlobal = isGlobal;
    }
    
    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final TraciValue value = exprNode.eval(context);
        
        if (isGlobal)
        {
            context.putGlobalValue(id, value);
        }
        else
        {
            context.putLocalValue(id, value);
        }
        
        return null;
    }

}