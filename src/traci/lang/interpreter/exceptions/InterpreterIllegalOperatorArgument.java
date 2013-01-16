package traci.lang.interpreter.exceptions;

import traci.lang.interpreter.CallStack;
import traci.lang.interpreter.TraciValue.Type;
import traci.lang.parser.IncludeLocation;

@SuppressWarnings("serial")
public class InterpreterIllegalOperatorArgument extends InterpreterRuntimeException
{
    public final Type leftType;
    public final String op;
    public final Type rightType;

    private static String makeMsg(final Type leftType, final String op, final Type rightType)
    {
        final StringBuilder sb = new StringBuilder();

        sb.append("Unable to evaluate expression '");

        if (leftType != null)
        {
            sb.append(leftType.toString()).append(' ');
        }

        sb.append(op);

        if (rightType != null)
        {
            sb.append(' ').append(rightType.toString());
        }

        sb.append("'");

        return sb.toString();
    }

    public InterpreterIllegalOperatorArgument(final IncludeLocation location, final CallStack callStack,
            final Type leftType, final String op, final Type rightType)
    {
        super(location, callStack, makeMsg(leftType, op, rightType));

        assert (leftType != null) || (rightType != null);
        assert op != null;

        this.leftType = leftType;
        this.op = op;
        this.rightType = rightType;
    }
}
