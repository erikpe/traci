package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;
import traci.model.material.Color;

public class ColorNode implements TraciNode
{
    private final TraciNode exprNode;
    
    public ColorNode(final TraciNode exprNode)
    {
        this.exprNode = exprNode;
    }
    
    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final TraciValue exprValue = exprNode.eval(context);
        final Color color = Color.make(exprValue.getVector());
        
        return new TraciValue(color);
    }
}
