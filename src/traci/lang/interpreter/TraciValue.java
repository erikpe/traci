package traci.lang.interpreter;

import traci.math.Transformation;
import traci.math.Vector;
import traci.model.material.Finish;
import traci.model.material.Material;
import traci.model.material.Texture;
import traci.model.material.pigment.Pigment;
import traci.model.shape.BoundingBox;
import traci.model.shape.Shape;
import traci.model.shape.csg.Csg;
import traci.model.shape.primitive.Primitive;

public class TraciValue implements Cloneable
{
    public static enum Type {
        NUMBER,
        BOOLEAN,
        VECTOR,
        PRIMITIVE_SHAPE,
        CSG_SHAPE,
        BOUNDING_BOX,
        TRANSFORMATION,
        MATERIAL,
        TEXTURE,
        FINISH,
        PIGMENT
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
        else if (obj instanceof BoundingBox)
        {
            type = Type.BOUNDING_BOX;
        }
        else if (obj instanceof Transformation)
        {
            type = Type.TRANSFORMATION;
        }
        else if (obj instanceof Material)
        {
            type = Type.MATERIAL;
        }
        else if (obj instanceof Texture)
        {
            type = Type.TEXTURE;
        }
        else if (obj instanceof Finish)
        {
            type = Type.FINISH;
        }
        else if (obj instanceof Pigment)
        {
            type = Type.PIGMENT;
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
    
    public BoundingBox getBoundingBox()
    {
        return (BoundingBox) value;
    }
    
    public Transformation getTransformation()
    {
        return (Transformation) value;
    }
    
    public Material getMaterial()
    {
        return (Material) value;
    }
    
    public Texture getTexture()
    {
        return (Texture) value;
    }
    
    public Finish getFinish()
    {
        return (Finish) value;
    }
    
    public Pigment getPigment()
    {
        return (Pigment) value;
    }
    
    @Override
    public String toString()
    {
        return value.toString();
    }
    
    @Override
    public Object clone()
    {
        switch (type)
        {
        case NUMBER:
        case BOOLEAN:
        case VECTOR:
        case TRANSFORMATION:
        case MATERIAL:
        case TEXTURE:
        case FINISH:
        case PIGMENT:
            return this;
            
        case PRIMITIVE_SHAPE:
            return new TraciValue(getPrimitive().clone());
            
        case CSG_SHAPE:
            return new TraciValue(getCsg().clone());
            
        case BOUNDING_BOX:
            return new TraciValue(getBoundingBox().clone());
            
        default:
            return null;
        }
    }
}
