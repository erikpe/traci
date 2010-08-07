package traci.render;

import traci.math.ObjectPool;
import traci.model.shape.primitive.Primitive;

public class Point2
{
    public static enum Type
    {
        ENTER,
        LEAVE,
        INTERSECT
    }
    
    public static final class Point2Pool extends ObjectPool<Point2>
    {
        @Override
        protected Point2 makeNew()
        {
            return new Point2(0, null, Type.INTERSECT);
        }
        
        private final Point2 make(final double dist, final Primitive obj, final Type type)
        {
            final Point2 p = getFree();
            
            p.dist = dist;
            p.obj = obj;
            p.type = type;
            
            return p;
        }
    }
    
    double dist;
    
    Primitive obj;
    
    Type type;
    
    private Point2(final double dist, final Primitive obj, final Type type)
    {
        this.dist = dist;
        this.obj = obj;
        this.type = type;
    }
    
    public static Point2 make(final double dist, final Primitive obj, final Type type)
    {
//        final Thread thisThread = Thread.currentThread();
//        
//        if (thisThread instanceof RenderingThread)
//        {
//            return ((RenderingThread) thisThread).point2Pool.make(dist, obj, type);
//        }
        
        return new Point2(dist, obj, type);
    }
    
    public double dist()
    {
        return dist;
    }
    
    public Primitive obj()
    {
        return obj;
    }
    
    public Type type()
    {
        return type;
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
        
        return new Point2(dist, obj, newType);
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
