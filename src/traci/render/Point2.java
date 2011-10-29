package traci.render;

import traci.model.shape.primitive.Primitive;

public class Point2
{
    public static enum Type
    {
        ENTER,
        LEAVE,
        INTERSECT
    }

    public final double dist;
    public final Primitive obj;
    public final Type type;

    private Point2(final double dist, final Primitive obj, final Type type)
    {
        this.dist = dist;
        this.obj = obj;
        this.type = type;
    }

    public static Point2 make(final double dist, final Primitive obj, final Type type)
    {
        return new Point2(dist, obj, type);
    }

    public Point2 invert()
    {
        Type newType = null;

        switch (type)
        {
        case ENTER: newType = Type.LEAVE; break;
        case LEAVE: newType = Type.ENTER; break;
        default: assert false;
        }

        return Point2.make(dist, obj, newType);
    }

    @Override
    public String toString()
    {
        final String strType;

        switch (type)
        {
        case ENTER: strType = "E"; break;
        case LEAVE: strType  = "L"; break;
        case INTERSECT: strType = "I"; break;
        default: throw new RuntimeException();
        }

        return "[" + strType + ": " + dist + "]";
    }
}
