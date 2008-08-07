package traci.render;

import java.util.Queue;
import java.util.Random;

import traci.math.ObjectPool;
import traci.math.Vector;
import traci.math.Vector2D;
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
        
        public Vector make(final double x, final double y, final double z)
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
        
        public Color make(final double r, final double g, final double b)
        {
            final Color color = make();
            
            color.r = r;
            color.g = g;
            color.b = b;
            
            return color;
        }
    }
    
    public static class Vector2DPool extends ObjectPool<Vector2D>
    {
        @Override
        protected Vector2D makeNew()
        {
            return Vector2D.makeNew(0, 0);
        }
        
        public Vector2D make(final double x, final double y)
        {
            final Vector2D vec = make();
            
            vec.x = x;
            vec.y = y;
            
            return vec;
        }
    };
    
    private static int index = 0;
    
    final private Queue<WorkBlock> workQueue;
    final private Renderer renderer;
    
    final public VectorPool vectorPool;
    final public ColorPool colorPool;
    final public Vector2DPool vector2DPool;
    
    RenderingThread(final Renderer renderer, final Queue<WorkBlock> workQueue)
    {
        super("Rendering thread #" + (index++));
        
        this.renderer = renderer;
        this.workQueue = workQueue;
        
        this.vectorPool = new VectorPool();
        this.colorPool = new ColorPool();
        this.vector2DPool = new Vector2DPool();
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
    
    public void resetPools()
    {
        vectorPool.reset();
        colorPool.reset();
        vector2DPool.reset();
    }
}
