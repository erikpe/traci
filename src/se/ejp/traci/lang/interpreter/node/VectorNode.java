package se.ejp.traci.lang.interpreter.node;

import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.Token;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalArgumentType;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.parser.TraciToken;
import se.ejp.traci.math.Vector;

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
        final List<Double> values = new ArrayList<Double>(3);

        for (int i = 0; i < 3; ++i)
        {
            final TraciValue value = nodes.get(i).eval(context);

            if (value.getType() != Type.NUMBER)
            {
                throw new InterpreterIllegalArgumentType(token.location, context.callStack, "vector", Type.NUMBER,
                        value.getType(), i + 1);
            }

            values.add(value.getNumber());
        }

        return new TraciValue(Vector.make(values.get(0), values.get(1), values.get(2)));
    }
}
