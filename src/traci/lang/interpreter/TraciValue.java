package traci.lang.interpreter;

import traci.math.Vector;
import traci.model.shape.Shape;
import traci.model.shape.csg.Csg;
import traci.model.shape.primitive.Primitive;

public class TraciValue
{
    public static enum Type {
        NUMBER,
        BOOLEAN,
        VECTOR,
        PRIMITIVE_SHAPE,
        CSG_SHAPE,
        UNKNOWN
    };
    
    private final Object value;
    private final Type type;
    
    public TraciValue(final Object obj)
    {
        this.value = obj;
        
        if (obj instanceof Double)
        {
            type = Type.NUMBER;
        }
        else if (obj instanceof Boolean)
        {
            type = Type.BOOLEAN;
        }
        else if (obj instanceof Vector)
        {
            type = Type.VECTOR;
        }
        else if (obj instanceof Primitive)
        {
            type = Type.PRIMITIVE_SHAPE;
        }
        else if (obj instanceof Csg)
        {
            type = Type.CSG_SHAPE;
        }
        else
        {
            throw new RuntimeException();
        }
    }
    
    public Type getType()
    {
        return type;
    }
    
    public Object getValue()
    {
        return value;
    }
    
    public Double getNumber()
    {
        return (Double) value;
    }
    
    public Boolean getBoolean()
    {
        return (Boolean) value;
    }
    
    public Vector getVector()
    {
        return (Vector) value;
    }
    
    public Shape getShape()
    {
        return (Shape) value;
    }
    
    public Primitive getPrimitive()
    {
        return (Primitive) value;
    }
    
    public Csg getCsg()
    {
        return (Csg) value;
    }
}
