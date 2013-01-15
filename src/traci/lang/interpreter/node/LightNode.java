package traci.lang.interpreter.node;

import java.util.List;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterIllegalNumberOfArguments;
import traci.lang.interpreter.exceptions.InterpreterInternalException;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.parser.TraciToken;
import traci.model.light.Light;
import traci.model.light.PointLight;

public class LightNode implements TraciNode
{
    private static enum LightType
    {
        pointlight
    }

    private final LightType type;
    private final List<TraciNode> argNodes;
    private final BlockNode blockNode;
    private final TraciToken token;

    public LightNode(final String lightType, final List<TraciNode> argNodes, final BlockNode blockNode, final Token token)
    {
        this.type = LightType.valueOf(lightType);
        this.argNodes = argNodes;
        this.blockNode = blockNode;
        this.token = (TraciToken) token;
    }

    private PointLight makePointLight(final Context context) throws InterpreterRuntimeException
    {
        final int numArgs = (argNodes == null ? 0 : argNodes.size());

        if (numArgs != 2)
        {
            throw new InterpreterIllegalNumberOfArguments(token.location, context.callStack,
                    LightType.pointlight.toString(), 2, numArgs);
        }

        return new PointLight(null, null);
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        final Light light;

        switch (type)
        {
        case pointlight:
            light = makePointLight(context);
            break;

        default:
            throw new InterpreterInternalException("Unknown light type: " + type.toString());
        }

        final TraciValue value = new TraciValue(light);

        return new TraciValue(light);
    }
}
