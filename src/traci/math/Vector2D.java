package traci.math;

import traci.render.RenderingThread;

public class Vector2D
{
    public static class Vector2DPool extends ObjectPool<Vector2D>
    {
        @Override
        protected Vector2D makeNew()
        {
            return new Vector2D(0, 0);
        }
        
        public Vector2D make(final double x, final double y)
        {
            final Vector2D vec = getFree();
            
            vec.x = x;
            vec.y = y;
            
            return vec;
        }
    }
    
    double x;
    double y;
    
    Vector2D(final double x, final double y)
    {
        this.x = x;
        this.y = y;
    }
    
    public static Vector2D make(final double x, final double y)
    {
        final Thread thisThread = Thread.currentThread();
        
        if (thisThread instanceof RenderingThread)
        {
            return ((RenderingThread) thisThread).vector2DPool.make(x, y);
        }
        
        return new Vector2D(x, y);
    }
    
    public double x()
    {
        return x;
    }
    
    public double y()
    {
        return y;
    }
}
