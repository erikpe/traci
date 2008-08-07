package traci.math;

import traci.render.RenderingThread;

public class Vector2D
{
    public double x;
    public double y;
    
    private Vector2D(final double x, final double y)
    {
        this.x = x;
        this.y = y;
    }
    
    public static Vector2D make(final double x, final double y)
    {
        // return new Vector2D(x, y);
        
        final Thread thisThread = Thread.currentThread();
        
        if (thisThread instanceof RenderingThread)
        {
            return ((RenderingThread) thisThread).vector2DPool.make(x, y);
        }
        
        return new Vector2D(x, y);
    }
    
    public static Vector2D makeNew(final double x, final double y)
    {
        return new Vector2D(x, y);
    }
}
