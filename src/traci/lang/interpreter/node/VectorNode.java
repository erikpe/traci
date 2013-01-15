package traci.lang.interpreter.node;

import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.TraciValue.Type;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterIllegalArgumentType;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.parser.TraciToken;
import traci.math.Vector;

public class VectorNode implements TraciNode
{
    private final List<TraciNode> nodes;
    private final TraciToken token;

    public VectorNode(final TraciNode aNode, final TraciNode bNode, final TraciNode cNode, final Token token)
    {
        this.nodes = new ArrayList<TraciNode>(3);
        this.token = (TraciToken) token;

        nodes.add(aNode);
        nodes.add(bNode);
        nodes.add(cNode);
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        final List<TraciValue> values = new ArrayList<TraciValue>(3);

        for (int i = 0; i < 3; ++i)
        {
            final TraciValue value = nodes.get(i).eval(context);
            values.add(value);

            if (value.getType() != Type.NUMBER)
            {
                throw new InterpreterIllegalArgumentType(token.location, context.callStack,
                        "vector", Type.NUMBER, value.getType(), i + 1);
            }
        }

        return new TraciValue(Vector.make(values.get(0).getNumber(), values.get(1).getNumber(), values.get(2).getNumber()));
    }
}
