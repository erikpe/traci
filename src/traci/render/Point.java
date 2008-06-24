package traci.render;

import traci.math.Matrix;
import traci.math.Vector;
import traci.model.shape.Primitive;

public class Point
{
    public final double dist;
    
    public final Primitive obj;
    
    public Vector normal;
    
    public Point(final double dist, final Primitive obj, final Vector normal)
    {
        this.dist = dist;
        this.obj = obj;
        this.normal = normal;
    }
    
    public static Point nearest(final Point p0, final Point p1)
    {
        return (p0.dist < p1.dist ? p0 : p1);
    }
    
    public static Point farest(final Point p0, final Point p1)
    {
        return (p0.dist > p1.dist ? p0 : p1);
    }
    
    public void transformNormal(final Matrix mat)
    {
        normal = mat.mul(normal);
    }
    
    public Point invNormal()
    {
        return new Point(dist, obj, normal.neg());
    }
    
    public String toString()
    {
        return "[" + dist + " " + normal + "]";
    }
}
