package traci.lang.interpreter.node;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.FunctionReturnException;
import traci.lang.interpreter.InterpreterRuntimeException;
import traci.lang.interpreter.TraciValue;

public class ReturnNode implements TraciNode
{
    private final TraciNode exprNode;

    public ReturnNode(final TraciNode exprNode)
    {
        this.exprNode = exprNode;
    }

    @Override
    public TraciValue eval(final Context context) throws FunctionReturnException, InterpreterRuntimeException
    {
        final TraciValue value = exprNode.eval(context);
        throw new FunctionReturnException(value);
    }
}
