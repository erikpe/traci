package se.ejp.traci.render;

import se.ejp.traci.math.Vector;
import se.ejp.traci.model.shape.primitive.Primitive;

public class Point
{
    public static enum Type
    {
        ENTER,
        LEAVE,
        INTERSECT;
    }

    public final double dist;
    public final Primitive obj;
    public final Type type;
    public final Vector normal;

    private Point(final double dist, final Primitive obj, final Type type, final Vector normal)
    {
        this.dist = dist;
        this.obj = obj;
        this.type = type;
        this.normal = normal;
    }

    public static Point make(final double dist, final Primitive obj, final Type type, final Vector normal)
    {
        return new Point(dist, obj, type, normal);
    }

    public Point invert()
    {
        Type newType = null;

        switch (type)
        {
        case ENTER: newType = Type.LEAVE; break;
        case LEAVE: newType = Type.ENTER; break;
        default: /* INTERSECT */ break;
        }

        return Point.make(dist, obj, newType, normal);
    }

    @Override
    public String toString()
    {
        final String strType;

        switch (type)
        {
        case ENTER: strType = "E"; break;
        case LEAVE: strType  = "L"; break;
        default: /* INTERSECT */ strType = "I"; break;
        }

        return "[" + strType + ": " + dist + "]";
    }
}
