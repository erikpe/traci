package traci.lang.interpreter;

import java.util.HashMap;

@SuppressWarnings("serial")
public class Functions extends HashMap<String, Function>
{
    public Functions()
    {
        super();
        
        put("print", BuiltinFunctions.FUNCTION_PRINT);
    }
    
    public Functions(final Functions other)
    {
        super(other);
    }
}
