package traci.lang.interpreter.node;

import java.util.List;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.model.shape.csg.Csg;
import traci.model.shape.csg.Difference;
import traci.model.shape.csg.Intersection;
import traci.model.shape.csg.Union;

public class CsgShapeNode implements TraciNode
{
    private static enum CsgType
    {
        UNION("union")
        {
            @Override
            protected Union make()
            {
                return new Union();
            }
        },

        DIFFERENCE("difference")
        {
            @Override
            protected Difference make()
            {
                return new Difference();
            }
        },

        INTERSECTION("intersection")
        {
            @Override
            protected Intersection make()
            {
                return new Intersection();
            }
        };

        private final String id;

        protected abstract Csg make();

        private CsgType(final String id)
        {
            this.id = id;
        }

        @Override
        public String toString()
        {
            return id;
        }
    }

    private final CsgType csgType;
    private final BlockNode blockNode;

    public CsgShapeNode(final String shapeType, final List<TraciNode> argNodes, final BlockNode blockNode)
    {
        this.csgType = CsgType.valueOf(shapeType.toUpperCase());
        this.blockNode = blockNode;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        final Csg csg = csgType.make();
        TraciValue value = new TraciValue(csg);

        if (blockNode != null)
        {
            final Entity entity = Entities.makeEntity(csg);
            blockNode.eval(context.newEntity(entity));
            value = entity.getValue();
            assert csg == value.getObject();
        }

        return value;
    }
}
