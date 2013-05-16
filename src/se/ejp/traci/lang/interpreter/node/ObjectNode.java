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
import se.ejp.traci.model.light.AmbientLight;
import se.ejp.traci.model.light.PointLight;
import se.ejp.traci.model.shape.BoundingBox;
import se.ejp.traci.model.shape.csg.Difference;
import se.ejp.traci.model.shape.csg.Intersection;
import se.ejp.traci.model.shape.csg.Union;
import se.ejp.traci.model.shape.primitive.Box;
import se.ejp.traci.model.shape.primitive.Cylinder;
import se.ejp.traci.model.shape.primitive.Plane;
import se.ejp.traci.model.shape.primitive.Sphere;
import se.ejp.traci.model.shape.primitive.Torus;

public class ObjectNode implements TraciNode
{
    static enum ObjectType
    {
        BOX("box", Box.class),
        CYLINDER("cylinder", Cylinder.class),
        PLANE("plane", Plane.class),
        SPHERE("sphere", Sphere.class),
        TORUS("torus", Torus.class),

        UNION("union", Union.class),
        DIFFERENCE("difference", Difference.class),
        INTERSECTION("intersection", Intersection.class),

        BBOX("bbox", BoundingBox.class),

        POINTLIGHT("pointlight", PointLight.class),
        AMBIENTLIGHT("ambientlight", AmbientLight.class);

        final String id;
        final Class<?> clazz;

        private ObjectType(final String id, final Class<?> clazz)
        {
            this.id = id;
            this.clazz = clazz;
        }
    }

    private static final Map<String, ObjectType> typeMap;
    static
    {
        final Map<String, ObjectType> types = new HashMap<String, ObjectType>();
        for (final ObjectType objectType : ObjectType.values())
        {
            types.put(objectType.id, objectType);
        }
        typeMap = Collections.<String, ObjectType>unmodifiableMap(types);
    }

    final ObjectType objectType;
    final List<TraciNode> argNodes;
    final BlockNode blockNode;
    final TraciToken token;

    public ObjectNode(final String typeStr, final List<TraciNode> argNodes, final BlockNode blockNode, final Token token)
    {
        assert argNodes != null;
        this.objectType = typeMap.get(typeStr);
        this.argNodes = argNodes;
        this.blockNode = blockNode;
        this.token = (TraciToken) token;

        if (objectType == null)
        {
            throw new InterpreterInternalException("Unknown object type: " + typeStr);
        }
    }

    private static Object make(final ObjectType objectType, final List<TraciValue> traciArgs)
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
            final Constructor<?> constructor = objectType.clazz.getConstructor(argTypes);
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

        final Object object = make(objectType, args);
        if (object == null)
        {
            final List<Type> argTypes = new ArrayList<Type>(args.size());
            for (final TraciValue val : args)
            {
                argTypes.add(val.getType());
            }

            throw new InterpreterIllegalArguments(token.location, context.callStack, objectType.id, argTypes);
        }

        TraciValue value = new TraciValue(object);

        if (blockNode != null)
        {
            final Entity entity = Entities.makeEntity(value.getObject());
            blockNode.eval(context.newEntity(entity));
            value = entity.getValue();
            assert object == value.getObject();
        }

        return value;
    }
}
