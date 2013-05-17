package se.ejp.traci.lang.interpreter.node;

import java.lang.reflect.Method;
import java.util.ArrayList;
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
import se.ejp.traci.math.Transformations;
import se.ejp.traci.model.light.AmbientLight;
import se.ejp.traci.model.light.PointLight;
import se.ejp.traci.model.material.pigment.Checker;
import se.ejp.traci.model.material.pigment.FileImage;
import se.ejp.traci.model.material.pigment.Solid;
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
        BOX           ("box",          Box.class,             "make"),
        CYLINDER      ("cylinder",     Cylinder.class,        "make"),
        PLANE         ("plane",        Plane.class,           "make"),
        SPHERE        ("sphere",       Sphere.class,          "make"),
        TORUS         ("torus",        Torus.class,           "make"),
        UNION         ("union",        Union.class,           "make"),
        DIFFERENCE    ("difference",   Difference.class,      "make"),
        INTERSECTION  ("intersection", Intersection.class,    "make"),
        BBOX          ("bbox",         BoundingBox.class,     "make"),
        POINTLIGHT    ("pointlight",   PointLight.class,      "make"),
        AMBIENTLIGHT  ("ambientlight", AmbientLight.class,    "make"),
        IMAGE         ("image",        FileImage.class,       "make"),
        SOLID         ("solid",        Solid.class,           "make"),
        CHECKER       ("checker",      Checker.class,         "make"),
        IDENTITY      ("identity",     Transformations.class, "identity"),
        ROTX          ("rotx",         Transformations.class, "rotx"),
        ROTY          ("roty",         Transformations.class, "roty"),
        ROTZ          ("rotz",         Transformations.class, "rotz"),
        TRANSLATE     ("translate",    Transformations.class, "translate"),
        SCALE         ("scale",        Transformations.class, "scale"),
        SCALEX        ("scalex",       Transformations.class, "scalex"),
        SCALEY        ("scaley",       Transformations.class, "scaley"),
        SCALEZ        ("scalez",       Transformations.class, "scalez"),
        ROT_VEC_TO_VEC("rotVecToVec",  Transformations.class, "rotVecToVec"),
        ROT_AROUND    ("rotAround",    Transformations.class, "rotAround");

        final String id;
        final Class<?> clazz;
        final String methodName;

        private ObjectType(final String id, final Class<?> clazz, final String methodName)
        {
            this.id = id;
            this.clazz = clazz;
            this.methodName = methodName;
        }

        private static final Map<String, ObjectType> idMap = new HashMap<String, ObjectType>();
        static
        {
            for (final ObjectType objectType : ObjectType.values())
            {
                idMap.put(objectType.id, objectType);
            }
        }

        private static ObjectType get(final String id)
        {
            return idMap.get(id);
        }
    }

    final ObjectType objectType;
    final List<TraciNode> argNodes;
    final BlockNode blockNode;
    final TraciToken token;

    public ObjectNode(final String typeStr, final List<TraciNode> argNodes, final BlockNode blockNode, final Token token)
    {
        assert argNodes != null;
        this.objectType = ObjectType.get(typeStr);
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
        if (objectType == ObjectType.IMAGE)
        {
            final int i = 23;
        }
        final Class<?>[] argTypes = new Class<?>[traciArgs.size()];
        final Object[] args = new Object[traciArgs.size()];

        for (int i = 0; i < traciArgs.size(); ++i)
        {
            argTypes[i] = traciArgs.get(i).getType().clazz;
            args[i] = traciArgs.get(i).getObject();
        }

        try
        {
            final Method method = objectType.clazz.getMethod(objectType.methodName, argTypes);
            return method.invoke(null, args);
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
