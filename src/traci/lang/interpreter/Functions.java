package traci.lang.interpreter;

import java.util.HashMap;

import traci.lang.interpreter.node.FunctionNode;

@SuppressWarnings("serial")
public class Functions extends HashMap<String, FunctionNode>
{
    public Functions()
    {
        super();
    }
    
    public Functions(final Functions other)
    {
        super(other);
    }
}
