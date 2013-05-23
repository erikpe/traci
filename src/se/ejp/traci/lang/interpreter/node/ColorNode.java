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
import se.ejp.traci.model.material.Color;

public class ColorNode implements TraciNode
{
    final List<TraciNode> nodes;
    final TraciToken token;

    public ColorNode(final TraciNode r, final TraciNode g, final TraciNode b, final TraciNode transmit, final Token token)
    {
        this.nodes = new ArrayList<TraciNode>();
        this.token = (TraciToken) token;

        nodes.add(r);
        nodes.add(g);
        nodes.add(b);

        if (transmit != null)
        {
            nodes.add(transmit);
        }
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        final List<Double> values = new ArrayList<Double>();

        for (int i = 0; i < nodes.size(); ++i)
        {
            final TraciValue value = nodes.get(i).eval(context);

            if (value.getType() != Type.NUMBER)
            {
                throw new InterpreterIllegalArgumentType(token.location, context.callStack, "color", Type.NUMBER,
                        value.getType(), i + 1);
            }

            values.add(value.getNumber());
        }

        if (values.size() == 4)
        {
            return new TraciValue(Color.make(values.get(0), values.get(1), values.get(2), values.get(3)));
        }

        return new TraciValue(Color.make(values.get(0), values.get(1), values.get(2)));
    }
}
