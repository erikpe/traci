package traci.render;

import traci.math.ObjectPool;
import traci.model.shape.primitive.Primitive;

@Deprecated
public class Point
{
    public static final class PointPool extends ObjectPool<Point>
    {
        @Override
        protected Point makeNew()
        {
            return new Point(0, null);
        }
        
        private final Point make(final double dist, final Primitive obj)
        {
            final Point p = getFree();
            
            p.dist = dist;
            p.obj = obj;
            
            return p;
        }
    }
    
    private double dist;
    
    private Primitive obj;
    
    private Point(final double dist, final Primitive obj)
    {
        this.dist = dist;
        this.obj = obj;
    }
    
    public static Point make(final double dist, final Primitive obj)
    {
        final Thread thisThread = Thread.currentThread();
        
        if (thisThread instanceof RenderingThread)
        {
            return ((RenderingThread) thisThread).pointPool.make(dist, obj);
        }
        
        return new Point(dist, obj);
    }
    
    public double dist()
    {
        return dist;
    }
    
    public Primitive obj()
    {
        return obj;
    }
    
    public static Point nearest(final Point p0, final Point p1)
    {
        return p0.dist < p1.dist ? p0 : p1;
    }
    
    public static Point farest(final Point p0, final Point p1)
    {
        return p0.dist > p1.dist ? p0 : p1;
    }
    
    @Override
    public String toString()
    {
        return "[" + dist + "]";
    }
}
