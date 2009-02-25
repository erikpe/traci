package traci.render;

import traci.math.ObjectPool;

public class Interval
{
    public static class IntervalPool extends ObjectPool<Interval>
    {
        @Override
        protected Interval makeNew()
        {
            return new Interval(null, null);
        }
        
        public Interval make(final Point p0, final Point p1)
        {
            final Interval ival = getFree();
            
            ival.p0 = p0;
            ival.p1 = p1;
            
            return ival;
        }
    }
    
    Point p0;
    Point p1;
    
    Interval(final Point p0, final Point p1)
    {
        assert p0 == null || p1 == null || p0.dist <= p1.dist;
        assert (p0 == null) == (p1 == null);
        
        this.p0 = p0;
        this.p1 = p1;
    }
    
    public static Interval make(final Point p0, final Point p1)
    {
        final Thread thisThread = Thread.currentThread();
        
        if (thisThread instanceof RenderingThread)
        {
            return ((RenderingThread) thisThread).intervalPool.make(p0, p1);
        }
        
        return new Interval(p0, p1);
    }
    
    public Point p0()
    {
        return p0;
    }
    
    public Point p1()
    {
        return p1;
    }
}
