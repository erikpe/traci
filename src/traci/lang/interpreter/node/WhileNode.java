package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.InterpreterRuntimeException;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.TraciValue.Type;

public class WhileNode implements TraciNode
{
    private final TraciNode condNode;
    private final BlockNode blockNode;

    public WhileNode(final TraciNode condNode, final BlockNode blockNode)
    {
        this.condNode = condNode;
        this.blockNode = blockNode;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        TraciValue condValue;

        do
        {
            condValue = condNode.eval(context);

            if (condValue.getType() != Type.BOOLEAN)
            {
                throw new RuntimeException("must be bool");
            }

            if (condValue.getBoolean())
            {
                blockNode.eval(context);
            }
        } while (condValue.getBoolean());

        return null;
    }
}
