package traci.lang.interpreter.node;

import java.util.List;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.model.shape.BoundingBox;

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
            context.pushEntity(entity);
            try
            {
                blockNode.eval(context);
            }
            catch (final FunctionReturnException e)
            {
                throw e;
            }
            finally
            {
                context.popEntity();
            }
            value = entity.getValue();
            assert bBox == value.getObject();
        }

        return value;
    }
}
