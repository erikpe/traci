package traci.lang.interpreter.node;

import java.util.EnumSet;
import java.util.Set;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.TraciValue.Type;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterIllegalArgumentType;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.parser.TraciToken;
import traci.math.Transformation;
import traci.math.Transformations;

public class TransformationNode implements TraciNode
{
    private static enum TransformationType
    {
        rotx(EnumSet.<Type>of(Type.NUMBER))
        {
            @Override
            protected Transformation make(final TraciValue value)
            {
                return Transformations.rotx(value.getNumber());
            }
        },

        roty(EnumSet.<Type>of(Type.NUMBER))
        {
            @Override
            protected Transformation make(final TraciValue value)
            {
                return Transformations.roty(value.getNumber());
            }
        },

        rotz(EnumSet.<Type>of(Type.NUMBER))
        {
            @Override
            protected Transformation make(final TraciValue value)
            {
                return Transformations.rotz(value.getNumber());
            }
        },

        translate(EnumSet.<Type>of(Type.VECTOR))
        {
            @Override
            protected Transformation make(final TraciValue value)
            {
                return Transformations.translate(value.getVector());
            }
        },

        scale(EnumSet.<Type>of(Type.NUMBER, Type.VECTOR))
        {
            @Override
            protected Transformation make(final TraciValue value)
            {
                if (value.getType() == Type.NUMBER)
                {
                    return Transformations.scale(value.getNumber());
                }
                else
                {
                    return Transformations.scale(value.getVector());
                }
            }
        };

        private final Set<Type> validTypes;

        protected abstract Transformation make(final TraciValue value);

        private TransformationType(final Set<Type> validTypes)
        {
            this.validTypes = validTypes;
        }
    }

    private final TransformationType transformationType;
    private final TraciNode exprNode;
    private final TraciToken token;

    public TransformationNode(final String typeStr, final TraciNode exprNode, final Token token)
    {
        this.transformationType = TransformationType.valueOf(typeStr);
        this.exprNode = exprNode;
        this.token = (TraciToken) token;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        final TraciValue exprValue = exprNode.eval(context);

        if (!transformationType.validTypes.contains(exprValue.getType()))
        {
            throw new InterpreterIllegalArgumentType(token.location, context.callStack, transformationType.toString(),
                    transformationType.validTypes, exprValue.getType(), 1);
        }

        return new TraciValue(transformationType.make(exprValue));
    }
}
