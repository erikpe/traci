package traci.render;

import traci.math.ObjectPool;
import traci.model.shape.primitive.Primitive;

public class IntersectionStack
{
    public static final class IntersectionStackPool extends ObjectPool<IntersectionStack>
    {
        @Override
        protected final IntersectionStack makeNew()
        {
            return new IntersectionStack();
        }
        
        private final IntersectionStack make()
        {
            final IntersectionStack iStack = getFree();
            
            iStack.reset();
            
            return iStack;
        }
    }
    
    private static final int SIZE = 32;
    
    public final double[] dists;
    public final Primitive[] objs;
    
    private int ptr;
    
    private IntersectionStack()
    {
        this.dists = new double[SIZE];
        this.objs = new Primitive[SIZE];
        this.ptr = 0;
    }
    
    public static IntersectionStack make()
    {
        final Thread thisThread = Thread.currentThread();
        
        if (thisThread instanceof RenderingThread)
        {
            return ((RenderingThread) thisThread).intersectionStackPool.make();
        }
        
        throw new IllegalStateException();
    }
    
    public int size()
    {
        return ptr;
    }
    
    public boolean isEmpty()
    {
        return ptr == 0;
    }
    
    public void push(final double dist, final Primitive obj)
    {
        dists[ptr] = dist;
        objs[ptr++] = obj;
    }
    
    public void reset()
    {
        ptr = 0;
    }
}
