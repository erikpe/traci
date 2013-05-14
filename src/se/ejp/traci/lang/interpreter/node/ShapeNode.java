package se.ejp.traci.lang.interpreter.node;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.antlr.runtime.Token;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.Entities;
import se.ejp.traci.lang.interpreter.Entities.Entity;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalArguments;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterInternalException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.parser.TraciToken;
import se.ejp.traci.model.shape.Shape;
import se.ejp.traci.model.shape.csg.Difference;
import se.ejp.traci.model.shape.csg.Intersection;
import se.ejp.traci.model.shape.csg.Union;
import se.ejp.traci.model.shape.primitive.Box;
import se.ejp.traci.model.shape.primitive.Cylinder;
import se.ejp.traci.model.shape.primitive.Plane;
import se.ejp.traci.model.shape.primitive.Sphere;
import se.ejp.traci.model.shape.primitive.Torus;

public class ShapeNode implements TraciNode
{
    static enum ShapeType
    {
        BOX("box", Box.class),
        CYLINDER("cylinder", Cylinder.class),
        PLANE("plane", Plane.class),
        SPHERE("sphere", Sphere.class),
        TORUS("torus", Torus.class),
        UNION("union", Union.class),
        DIFFERENCE("difference", Difference.class),
        INTERSECTION("intersection", Intersection.class);

        final String id;
        final Class<? extends Shape> clazz;

        private ShapeType(final String id, final Class<? extends Shape> clazz)
        {
            this.id = id;
            this.clazz = clazz;
        }
    }

    private static final Map<String, ShapeType> typeMap;
    static
    {
        final Map<String, ShapeType> types = new HashMap<String, ShapeType>();
        for (final ShapeType transformationType : ShapeType.values())
        {
            types.put(transformationType.id, transformationType);
        }
        typeMap = Collections.<String, ShapeType>unmodifiableMap(types);
    }

    final ShapeType shapeType;
    final List<TraciNode> argNodes;
    final BlockNode blockNode;
    private final TraciToken token;

    public ShapeNode(final String typeStr, final List<TraciNode> argNodes, final BlockNode blockNode, final Token token)
    {
        this.shapeType = typeMap.get(typeStr);
        this.argNodes = (argNodes == null ? Collections.<TraciNode>emptyList() : argNodes);
        this.blockNode = blockNode;
        this.token = (TraciToken) token;

        if (shapeType == null)
        {
            throw new InterpreterInternalException("Unknown shape type: " + typeStr);
        }
    }

    private static Shape make(final ShapeType shapeType, final List<TraciValue> traciArgs)
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
            final Constructor<? extends Shape> constructor = shapeType.clazz.getConstructor(argTypes);
            return constructor.newInstance(args);
        }
        catch (final Exception e)
        {
            return null;
        }
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        final List<TraciValue> args = new ArrayList<TraciValue>(argNodes.size());
        for (final TraciNode argNode : argNodes)
        {
            args.add(argNode.eval(context));
        }

        final Shape shape = make(shapeType, args);
        if (shape == null)
        {
            final List<Type> argTypes = new ArrayList<Type>(args.size());
            for (final TraciValue val : args)
            {
                argTypes.add(val.getType());
            }

            throw new InterpreterIllegalArguments(token.location, context.callStack, shapeType.id, argTypes);
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
