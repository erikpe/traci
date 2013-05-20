package se.ejp.traci.lang.interpreter;

import se.ejp.traci.lang.interpreter.exceptions.InterpreterInternalException;
import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.light.Light;
import se.ejp.traci.model.material.Color;
import se.ejp.traci.model.material.Finish;
import se.ejp.traci.model.material.Material;
import se.ejp.traci.model.material.Texture;
import se.ejp.traci.model.material.pigment.Pigment;
import se.ejp.traci.model.shape.BoundingBox;
import se.ejp.traci.model.shape.Shape;
import se.ejp.traci.model.shape.csg.Csg;
import se.ejp.traci.model.shape.primitive.Primitive;

public class TraciValue implements Cloneable
{
    public static enum Type
    {
        NUMBER(Double.class),
        BOOLEAN(Boolean.class),
        VECTOR(Vector.class),
        STRING(String.class),
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
        Type tmpType = null;
        for (final Type t : Type.values())
        {
            if (t.clazz.isAssignableFrom(obj.getClass()))
            {
                tmpType = t;
                break;
            }
        }

        if (tmpType == null)
        {
            throw new InterpreterInternalException("Unable to make TraciValue of type: " + obj.getClass().toString());
        }

        this.value = obj;
        this.type = tmpType;
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

    public String getString()
    {
        return (String) value;
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
    public Object clone() throws CloneNotSupportedException
    {
        switch (type)
        {
        case NUMBER:
        case BOOLEAN:
        case VECTOR:
        case STRING:
        case TRANSFORMATION:
        case MATERIAL:
        case TEXTURE:
        case FINISH:
        case PIGMENT:
        case COLOR:
            return this; // These are immuatable, no need to clone

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
