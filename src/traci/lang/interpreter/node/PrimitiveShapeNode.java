package traci.lang.interpreter.node;

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
import traci.lang.interpreter.exceptions.InterpreterIllegalNumberOfArguments;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.parser.TraciToken;
import traci.model.shape.primitive.Box;
import traci.model.shape.primitive.Cylinder;
import traci.model.shape.primitive.Plane;
import traci.model.shape.primitive.Primitive;
import traci.model.shape.primitive.Sphere;
import traci.model.shape.primitive.Torus;

public class PrimitiveShapeNode implements TraciNode
{
    private static enum PrimitiveType
    {
        BOX(new Type[0])
        {
            @Override
            protected Primitive make(final List<TraciValue> args)
            {
                return new Box();
            }
        },

        CYLINDER(new Type[] { Type.NUMBER, Type.VECTOR, Type.VECTOR })
        {
            @Override
            protected Primitive make(final List<TraciValue> args)
            {
                return new Cylinder(args.get(0).getNumber(), args.get(1).getVector(), args.get(2).getVector());
            }
        },

        PLANE(new Type[0])
        {
            @Override
            protected Primitive make(final List<TraciValue> args)
            {
                return new Plane();
            }
        },

        SPHERE(new Type[0])
        {
            @Override
            protected Primitive make(final List<TraciValue> args)
            {
                return new Sphere();
            }
        },

        TORUS(new Type[] { Type.NUMBER, Type.NUMBER })
        {
            @Override
            protected Primitive make(final List<TraciValue> args)
            {
                return new Torus(args.get(0).getNumber(), args.get(1).getNumber());
            }
        };

        private Type[] expectedArgTypes;

        protected abstract Primitive make(final List<TraciValue> args);

        private PrimitiveType(final Type[] expectedArgTypes)
        {
            this.expectedArgTypes = expectedArgTypes;
        }
    }

    private final PrimitiveType primitiveType;
    private final List<TraciNode> argNodes;
    private final BlockNode blockNode;
    private final TraciToken token;

    public PrimitiveShapeNode(final String shapeType, final List<TraciNode> argNodes, final BlockNode blockNode,
            final Token token)
    {
        this.primitiveType = PrimitiveType.valueOf(shapeType.toUpperCase());
        this.argNodes = (argNodes == null ? Collections.<TraciNode> emptyList() : argNodes);
        this.blockNode = blockNode;
        this.token = (TraciToken) token;
    }

    private void verifyNumberOfArgs(final Context context) throws InterpreterIllegalNumberOfArguments
    {
        final int expectedNumArgs = primitiveType.expectedArgTypes.length;

        if (expectedNumArgs != argNodes.size())
        {
            throw new InterpreterIllegalNumberOfArguments(token.location, context.callStack, primitiveType.toString(),
                    expectedNumArgs, argNodes.size());
        }
    }

    private void verifyArgumentTypes(final Context context, final List<TraciValue> args)
            throws InterpreterIllegalArgumentType
    {
        for (int i = 0; i < args.size(); ++i)
        {
            final Type argType = args.get(i).getType();
            final Type expectedArgType = primitiveType.expectedArgTypes[i];

            if (argType != expectedArgType)
            {
                throw new InterpreterIllegalArgumentType(token.location, context.callStack, primitiveType.toString(),
                        expectedArgType, argType, i + 1);
            }
        }
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        verifyNumberOfArgs(context);

        final List<TraciValue> args = new ArrayList<TraciValue>(argNodes.size());
        for (final TraciNode argNode : argNodes)
        {
            args.add(argNode.eval(context));
        }

        verifyArgumentTypes(context, args);

        final Primitive primitive = primitiveType.make(args);
        TraciValue value = new TraciValue(primitive);

        if (blockNode != null)
        {
            final Entity entity = Entities.makeEntity(value.getObject());
            blockNode.eval(context.newEntity(entity));
            value = entity.getValue();
            assert primitive == value.getObject();
        }

        return value;
    }
}
