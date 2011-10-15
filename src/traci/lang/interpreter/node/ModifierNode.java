package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.ModifierValue;
import traci.lang.interpreter.TraciValue;

public class ModifierNode implements TraciNode
{
    private final String type;
    private final TraciNode exprNode;
    
    public ModifierNode(final String type, final TraciNode exprNode)
    {
        this.type = type;
        this.exprNode = exprNode;
    }
    
    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final TraciValue exprValue = exprNode.eval(context);
        final ModifierValue modifierValue = new ModifierValue(type, exprValue.getValue());
        return new TraciValue(modifierValue);
    }
}
