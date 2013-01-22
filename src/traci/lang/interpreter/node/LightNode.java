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
import traci.model.light.Light;
import traci.model.light.PointLight;

public class LightNode implements TraciNode
{
    private static enum LightType
    {
        POINTLIGHT("pointlight", new Type[] { Type.VECTOR, Type.COLOR })
        {
            @Override
            protected PointLight make(final List<TraciValue> args)
            {
                return new PointLight(args.get(0).getVector(), args.get(1).getColor());
            }
        };

        private final String id;
        private final Type[] expectedArgTypes;

        protected abstract Light make(final List<TraciValue> args);

        private LightType(final String id, final Type[] expectedArgTypes)
        {
            this.id = id;
            this.expectedArgTypes = expectedArgTypes;
        }

        @Override
        public String toString()
        {
            return id;
        }
    }

    private final LightType lightType;
    private final List<TraciNode> argNodes;
    private final BlockNode blockNode;
    private final TraciToken token;

    public LightNode(final String lightType, final List<TraciNode> argNodes, final BlockNode blockNode, final Token token)
    {
        this.lightType = LightType.valueOf(lightType);
        this.argNodes = (argNodes == null ? Collections.<TraciNode>emptyList() : argNodes);
        this.blockNode = blockNode;
        this.token = (TraciToken) token;
    }

    private void verifyNumberOfArgs(final Context context) throws InterpreterIllegalNumberOfArguments
    {
        final int expectedNumArgs = lightType.expectedArgTypes.length;

        if (expectedNumArgs != argNodes.size())
        {
            throw new InterpreterIllegalNumberOfArguments(token.location, context.callStack, lightType.toString(),
                    expectedNumArgs, argNodes.size());
        }
    }

    private void verifyArgumentTypes(final Context context, final List<TraciValue> args)
            throws InterpreterIllegalArgumentType
    {
        for (int i = 0; i < args.size(); ++i)
        {
            final Type argType = args.get(i).getType();
            final Type expectedArgType = lightType.expectedArgTypes[i];

            if (argType != expectedArgType)
            {
                throw new InterpreterIllegalArgumentType(token.location, context.callStack, lightType.toString(),
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

        final Light light = lightType.make(args);
        TraciValue value = new TraciValue(light);

        if (blockNode != null)
        {
            final Entity entity = Entities.makeEntity(value.getObject());
            blockNode.eval(context.newEntity(entity));
            value = entity.getValue();
            assert light == value.getObject();
        }

        return value;
    }
}
