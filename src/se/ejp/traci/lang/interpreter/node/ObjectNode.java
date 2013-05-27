package se.ejp.traci.lang.interpreter.node;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
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
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.Camera;
import se.ejp.traci.model.Color;
import se.ejp.traci.model.light.AmbientLight;
import se.ejp.traci.model.light.PointLight;
import se.ejp.traci.model.material.Finish;
import se.ejp.traci.model.material.Interior;
import se.ejp.traci.model.material.Texture;
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
        ROT_AROUND    ("rotAround",    Transformations.class, "rotAround"),
        FINISH        ("finish",       Finish.class,          "make"),
        TEXTURE       ("texture",      Texture.class,         "make"),
        CAMERA        ("camera",       Camera.class,          "make"),
        INTERIOR      ("interior",     Interior.class,        "make"),
        COLOR         ("color",        Color.class,           "make"),
        VECTOR        ("vector[]",     Vector.class,          "make");

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
    }

    final ObjectType objectType;
    final List<TraciNode> argNodes;
    final BlockNode blockNode;
    final TraciToken token;

    private final Object[] argValues;
    private final Class<?>[] argClasses;
    private final Type[] argTypes;

    public ObjectNode(final String typeStr, final List<TraciNode> argNodes, final BlockNode blockNode, final Token token)
    {
        this.objectType = ObjectType.idMap.get(typeStr);
        this.argNodes = argNodes;
        this.blockNode = blockNode;
        this.token = (TraciToken) token;

        this.argValues = new Object[argNodes.size()];
        this.argClasses = new Class<?>[argNodes.size()];
        this.argTypes = new Type[argNodes.size()];

        if (objectType == null)
        {
            throw new InterpreterInternalException("Unknown object type: " + typeStr);
        }
    }

    private void evalArgNodes(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        for (int i = 0; i < argNodes.size(); ++i)
        {
            final TraciValue value = argNodes.get(i).eval(context);
            argValues[i] = value.getObject();
            argClasses[i] = value.getType().clazz;
            argTypes[i] = value.getType();
        }
    }

    private Object make(final Method method, final Context context) throws InterpreterRuntimeException
    {
        try
        {
            return method.invoke(null, argValues);
        }
        catch (final InvocationTargetException e)
        {
            final Throwable cause = e.getCause();

            if (cause instanceof InterpreterRuntimeException)
            {
                final InterpreterRuntimeException ire = (InterpreterRuntimeException) cause;
                ire.setLocation(token.location);
                ire.setCallStack(context.callStack);
                throw ire;
            }
            else if (cause instanceof InterpreterInternalException)
            {
                throw (InterpreterInternalException) cause;
            }

            throw new InterpreterInternalException(cause);
        }
        catch (final Exception e)
        {
            throw new InterpreterInternalException(e);
        }
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        evalArgNodes(context);

        final Method method;
        try
        {
            method = objectType.clazz.getMethod(objectType.methodName, argClasses);
        }
        catch (final NoSuchMethodException e)
        {
            throw new InterpreterIllegalArguments(token.location, context.callStack, objectType.id,
                    Arrays.asList(argTypes));
        }

        final Object object = make(method, context);
        TraciValue value = new TraciValue(object);

        if (blockNode != null)
        {
            final Entity entity = Entities.makeEntity(value.getObject());
            blockNode.eval(context.newEntity(entity));
            value = entity.getValue();
        }

        return value;
    }
}
