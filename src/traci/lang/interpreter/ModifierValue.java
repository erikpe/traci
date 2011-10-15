package traci.lang.interpreter;

import traci.math.Vector;

public class ModifierValue
{
    public static enum Type
    {
        ROTATE("rotate"),
        ROTX("rotx"),
        ROTY("roty"),
        ROTZ("rotz"),
        TRANSLATE("translate"),
        SCALE("scale"),
        COLOR("color"),
        UNKNOWN("unknown");
        
        public String typeString;
        
        private Type(final String typeString)
        {
            this.typeString = typeString;
        }
        
        private static Type getType(final String typeString)
        {
            for (final Type type : Type.values())
            {
                if (type.typeString.equals(typeString))
                {
                    return type;
                }
            }
            
            return Type.UNKNOWN;
        }
    }
    
    public final Type type;
    private final Object value;
    
    public ModifierValue(final String typeString, final Object value)
    {
        this.type = Type.getType(typeString);
        this.value = value;
    }
    
    public Object getValue()
    {
        return value;
    }
    
    public Vector getVector()
    {
        return (Vector) value;
    }
    
    public Double getNumber()
    {
        return (Double) value;
    }
}
