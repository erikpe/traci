package se.ejp.traci.lang.interpreter.node;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.antlr.runtime.Token;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalArguments;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterInternalException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.parser.TraciToken;
import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Transformations;

public class TransformationNode implements TraciNode
{
    private static enum TransformationType
    {
        ROTX("rotx"),
        ROTY("roty"),
        ROTZ("rotz"),
        TRANSLATE("translate"),
        SCALE("scale"),
        SCALEX("scalex"),
        SCALEY("scaley"),
        SCALEZ("scalez"),
        ROT_VEC_TO_VEC("rotVecToVec"),
        ROT_AROUND("rotAround");

        private final String id;

        private TransformationType(final String id)
        {
            this.id = id;
        }
    }

    private static final Map<String, TransformationType> typeMap;
    static
    {
        final Map<String, TransformationType> types = new HashMap<String, TransformationType>();
        for (final TransformationType transformationType : TransformationType.values())
        {
            types.put(transformationType.id, transformationType);
        }
        typeMap = Collections.<String, TransformationType>unmodifiableMap(types);
    }

    private final TransformationType transformationType;
    private final List<TraciNode> argNodes;
    private final BlockNode blockNode;
    private final TraciToken token;

    public TransformationNode(final String typeStr, final List<TraciNode> argNodes, final BlockNode blockNode,
            final Token token)
    {
        this.transformationType = typeMap.get(typeStr);
        this.argNodes = (argNodes == null ? Collections.<TraciNode>emptyList() : argNodes);
        this.blockNode = blockNode;
        this.token = (TraciToken) token;

        if (transformationType == null)
        {
            throw new InterpreterInternalException("Unknown transformation type: " + typeStr);
        }
    }

    private Transformation make(final TransformationType transformationType, final List<TraciValue> traciArgs)
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
            final Method method = Transformations.class.getMethod(transformationType.id, argTypes);
            return (Transformation) method.invoke(null, args);
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

        final Transformation transformation = make(transformationType, args);
        if (transformation == null)
        {
            final List<Type> argTypes = new ArrayList<Type>(args.size());
            for (final TraciValue val : args)
            {
                argTypes.add(val.getType());
            }

            throw new InterpreterIllegalArguments(token.location, context.callStack, transformationType.id, argTypes);
        }

        return new TraciValue(transformation);
    }
}
