package traci.lang.interpreter.node;

import org.antlr.runtime.Token;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.TraciValue.Type;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterIllegalArgumentType;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.parser.TraciToken;

public class ForNode implements TraciNode
{
    private final String counterId;
    private final TraciNode startNode;
    private final TraciNode endNode;
    private final BlockNode blockNode;
    private final TraciToken token;

    public ForNode(final String counterId, final TraciNode startNode, final TraciNode endNode,
            final BlockNode blockNode, final Token token)
    {
        this.counterId = counterId;
        this.startNode = startNode;
        this.endNode = endNode;
        this.blockNode = blockNode;
        this.token = (TraciToken) token;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        final TraciValue startValue = startNode.eval(context);

        if (startValue.getType() != Type.NUMBER)
        {
            throw new InterpreterIllegalArgumentType(token.location, context.callStack, "for-statement",
                    Type.NUMBER, startValue.getType(), 1);
        }

        final TraciValue endValue = endNode.eval(context);

        if (endValue.getType() != Type.NUMBER)
        {
            throw new InterpreterIllegalArgumentType(token.location, context.callStack, "for-statement",
                    Type.NUMBER, endValue.getType(), 2);
        }

        Double counter = startValue.getNumber();
        final Double end = endValue.getNumber();

        while (counter <= end)
        {
            context.putLocalValue(counterId, new TraciValue(counter));
            blockNode.eval(context);
            counter++;
        }

        return null;
    }
}
