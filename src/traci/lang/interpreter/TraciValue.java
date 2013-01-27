package traci.lang.interpreter;

import traci.lang.interpreter.exceptions.InterpreterInternalException;
import traci.math.Transformation;
import traci.math.Vector;
import traci.model.light.Light;
import traci.model.material.Color;
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
        NUMBER(Double.class),
        BOOLEAN(Boolean.class),
        VECTOR(Vector.class),
        PRIMITIVE_SHAPE(Primitive.class),
        CSG_SHAPE(Csg.class),
        BOUNDING_BOX(BoundingBox.class),
        TRANSFORMATION(Transformation.class),
        MATERIAL(Material.class),
        TEXTURE(Texture.class),
        FINISH(Finish.class),
        PIGMENT(Pigment.class),
        COLOR(Color.class),
        LIGHT(Light.class);

        public final Class<?> clazz;

        private Type(final Class<?> clazz)
        {
            this.clazz = clazz;
        }
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
        else if (obj instanceof Color)
        {
            type = Type.COLOR;
        }
        else if (obj instanceof Light)
        {
            type = Type.LIGHT;
        }
        else
        {
            throw new InterpreterInternalException("Unable to make TraciValue of type: " + obj.getClass().toString());
        }
    }

    public Type getType()
    {
        return type;
    }

    public Object getObject()
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

    public Color getColor()
    {
        return (Color) value;
    }

    public Light getLight()
    {
        return (Light) value;
    }

    @Override
    public String toString()
    {
        return "<" + type.toString() + ":" + value.toString() + ">";
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
        case COLOR:
            return this;

        case PRIMITIVE_SHAPE:
            return new TraciValue(getPrimitive().clone());

        case CSG_SHAPE:
            return new TraciValue(getCsg().clone());

        case BOUNDING_BOX:
            return new TraciValue(getBoundingBox().clone());

        default:
            throw new InterpreterInternalException("Trying to clone() value of type " + type.toString());
        }
    }
}
