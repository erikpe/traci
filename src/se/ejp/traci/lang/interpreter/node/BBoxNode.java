package se.ejp.traci.lang.interpreter.node;

import java.util.List;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.Entities;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.Entities.Entity;
import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.model.shape.BoundingBox;

public class BBoxNode implements TraciNode
{
    private final BlockNode blockNode;

    public BBoxNode(final List<TraciNode> argNodes, final BlockNode blockNode)
    {
        this.blockNode = blockNode;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        final BoundingBox bBox = new BoundingBox();

        TraciValue value = new TraciValue(bBox);

        if (blockNode != null)
        {
            final Entity entity = Entities.makeEntity(bBox);
            blockNode.eval(context.newEntity(entity));
            value = entity.getValue();
            assert bBox == value.getObject();
        }

        return value;
    }
}
