package traci.lang.interpreter.node;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.TraciValue.Type;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterIllegalArgumentType;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.parser.TraciToken;
import traci.model.shape.Shape;
import traci.model.shape.csg.Difference;
import traci.model.shape.csg.Intersection;
import traci.model.shape.csg.Union;
import traci.model.shape.primitive.Box;
import traci.model.shape.primitive.Cylinder;
import traci.model.shape.primitive.Plane;
import traci.model.shape.primitive.Sphere;
import traci.model.shape.primitive.Torus;

public class ShapeNode implements TraciNode
{
    private static enum ShapeType
    {
        BOX("box", Box.class),
        CYLINDER("cylinder", Cylinder.class),
        PLANE("plane", Plane.class),
        SPHERE("sphere", Sphere.class),
        TORUS("torus", Torus.class),
        UNION("union", Union.class),
        DIFFERENCE("difference", Difference.class),
        INTERSECTION("intersection", Intersection.class);

        private final String id;
        private final Class<? extends Shape> clazz;

        protected Shape make(final List<TraciValue> traciArgs)
        {
            final Class<?>[] argTypes = new Class<?>[traciArgs.size()];
            final Object[] args = new Object[traciArgs.size()];

            for (int i = 0; i < traciArgs.size(); ++i)
            {
                argTypes[i] = traciArgs.get(i).getType().clazz;
                args[i] = traciArgs.get(i).getObject();
            }

            try
            {
                final Constructor<? extends Shape> constructor = clazz.getConstructor(argTypes);
                return constructor.newInstance(args);
            }
            catch (final Exception e)
            {
                return null;
            }
        }

        private ShapeType(final String id, final Class<? extends Shape> clazz)
        {
            this.id = id;
            this.clazz = clazz;
        }

        @Override
        public String toString()
        {
            return id;
        }
    }

    private final ShapeType shapeType;
    private final List<TraciNode> argNodes;
    private final BlockNode blockNode;
    private final TraciToken token;

    public ShapeNode(final String shapeType, final List<TraciNode> argNodes, final BlockNode blockNode,
            final Token token)
    {
        this.shapeType = ShapeType.valueOf(shapeType.toUpperCase());
        this.argNodes = (argNodes == null ? Collections.<TraciNode> emptyList() : argNodes);
        this.blockNode = blockNode;
        this.token = (TraciToken) token;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        final List<TraciValue> args = new ArrayList<TraciValue>(argNodes.size());
        for (final TraciNode argNode : argNodes)
        {
            args.add(argNode.eval(context));
        }

        final Shape shape = shapeType.make(args);
        if (shape == null)
        {
            throw new InterpreterIllegalArgumentType(token.location, context.callStack, shapeType.toString(),
                    Type.BOOLEAN, Type.BOOLEAN, -1);
        }

        TraciValue value = new TraciValue(shape);

        if (blockNode != null)
        {
            final Entity entity = Entities.makeEntity(value.getObject());
            blockNode.eval(context.newEntity(entity));
            value = entity.getValue();
            assert shape == value.getObject();
        }

        return value;
    }
}
