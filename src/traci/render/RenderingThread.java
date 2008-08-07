package traci.render;

import java.util.Queue;
import java.util.Random;

import traci.math.ObjectPool;
import traci.math.Vector;
import traci.model.material.Color;

public class RenderingThread extends Thread
{
    public static class WorkBlock
    {
        public final long x;
        public final long y;
        public final long width;
        public final long height;
        
        public final Random randomSource;
        
        WorkBlock(final long x, final long y, final long width,
                final long height, final Random randomSource)
        {
            this.x = x;
            this.y = y;
            this.width = width;
            this.height = height;
            
            assert randomSource != null;
            this.randomSource = randomSource;
        }
    }
    
    public static class VectorPool extends ObjectPool<Vector>
    {
        @Override
        protected Vector makeNew()
        {
            return Vector.makeNew(0, 0, 0);
        }
        
        public Vector make(double x, double y, double z)
        {
            final Vector vec = make();
            
            vec.x = x;
            vec.y = y;
            vec.z = z;
            
            return vec;
        }
    };
    
    public static class ColorPool extends ObjectPool<Color>
    {
        @Override
        protected Color makeNew()
        {
            return Color.makeNew(0, 0, 0);
        }
        
        public Color make(double r, double g, double b)
        {
            final Color color = make();
            
            color.r = r;
            color.g = g;
            color.b = b;
            
            return color;
        }
    }
    
    private static int index = 0;
    
    final private Queue<WorkBlock> workQueue;
    final private Renderer renderer;
    
    final public VectorPool vectorPool;
    final public ColorPool colorPool;
    
    RenderingThread(final Renderer renderer, final Queue<WorkBlock> workQueue)
    {
        super("Rendering thread #" + (index++));
        
        this.renderer = renderer;
        this.workQueue = workQueue;
        
        this.vectorPool = new VectorPool();
        this.colorPool = new ColorPool();
    }
    
    @Override
    public void run()
    {
        WorkBlock block;
        
        while ((block = workQueue.poll()) != null)
        {
            renderer.renderBlock(block);
        }
    }
}
