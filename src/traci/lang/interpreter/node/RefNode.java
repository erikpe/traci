package traci.lang.interpreter.node;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;

public class RefNode implements TraciNode
{
    private final String id;
    private final BlockNode blockNode;
    private final Token token;
    
    public RefNode(final String id, final BlockNode blockNode, final Token token)
    {
        this.id = id;
        this.blockNode = blockNode;
        this.token = token;
    }
    
    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        TraciValue value = context.getValue(id);
        
        if (value == null)
        {
            System.out.println("Error: Undefined variable `" + id + "' at position " + token.getLine() + ":"
                    + token.getCharPositionInLine() + ".");
            throw new RuntimeException();
        }
        
        if (blockNode != null)
        {
            final Entity entity = Entities.makeEntity(value.getObject());
            blockNode.eval(context.newEntity(entity));
            value = entity.getValue();
        }
        
        return value;
    }
}
