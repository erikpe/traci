package se.ejp.traci.lang.interpreter.node;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;

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
