package se.ejp.traci.render;

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

    private Point(final double dist, final Primitive obj, final Type type)
    {
        this.dist = dist;
        this.obj = obj;
        this.type = type;
    }

    public static Point make(final double dist, final Primitive obj, final Type type)
    {
        return new Point(dist, obj, type);
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

        return Point.make(dist, obj, newType);
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
