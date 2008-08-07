package traci.render;

import traci.math.Matrix;
import traci.math.ObjectPool;
import traci.math.Vector;
import traci.model.shape.primitive.Primitive;

public class Point
{
    public static class PointPool extends ObjectPool<Point>
    {
        @Override
        protected Point makeNew()
        {
            return new Point(0, null, null);
        }
        
        public Point make(final double dist, final Primitive obj,
                final Vector normal)
        {
            final Point p = getFree();
            
            p.dist = dist;
            p.obj = obj;
            p.normal = normal;
            
            return p;
        }
    }
    
    public double dist;
    
    public Primitive obj;
    
    public Vector normal;
    
    private Point(final double dist, final Primitive obj, final Vector normal)
    {
        this.dist = dist;
        this.obj = obj;
        this.normal = normal;
    }
    
    public static Point make(final double dist, final Primitive obj,
            final Vector normal)
    {
        final Thread thisThread = Thread.currentThread();
        
        if (thisThread instanceof RenderingThread)
        {
            return ((RenderingThread) thisThread).pointPool.make(dist, obj, normal);
        }
        
        return new Point(dist, obj, normal);
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
    
    @Override
    public String toString()
    {
        return "[" + dist + " " + normal + "]";
    }
}
