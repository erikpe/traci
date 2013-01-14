package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.InterpreterRuntimeException;
import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.TraciValue.Type;

public class IfElseNode implements TraciNode
{
    private final TraciNode condNode;
    private final BlockNode ifBlock;
    private final BlockNode elseBlock;

    public IfElseNode(final TraciNode condNode, final BlockNode ifBlock, final BlockNode elseBlock)
    {
        this.condNode = condNode;
        this.ifBlock = ifBlock;
        this.elseBlock = elseBlock;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        final TraciValue condValue = condNode.eval(context);

        if (condValue.getType() != Type.BOOLEAN)
        {
            throw new RuntimeException("must be bool");
        }

        if (condValue.getBoolean())
        {
            ifBlock.eval(context);
        }
        else if (elseBlock != null)
        {
            elseBlock.eval(context);
        }

        return null;
    }
}
