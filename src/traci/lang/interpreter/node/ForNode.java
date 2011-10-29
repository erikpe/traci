package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.TraciValue;

public class ForNode implements TraciNode
{
    private final String counterId;
    private final TraciNode startNode;
    private final TraciNode endNode;
    private final BlockNode blockNode;

    public ForNode(final String counterId, final TraciNode startNode, final TraciNode endNode, final BlockNode blockNode)
    {
        this.counterId = counterId;
        this.startNode = startNode;
        this.endNode = endNode;
        this.blockNode = blockNode;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException
    {
        final TraciValue startValue = startNode.eval(context);
        final TraciValue endValue = endNode.eval(context);

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
