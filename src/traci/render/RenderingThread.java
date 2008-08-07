package traci.render;

import java.util.Queue;
import java.util.Random;

import traci.math.Vector.VectorPool;
import traci.math.Vector2D.Vector2DPool;
import traci.model.material.Color.ColorPool;
import traci.render.Interval.IntervalPool;
import traci.render.Point.PointPool;

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
    
    private static int index = 0;
    
    private final Queue<WorkBlock> workQueue;
    private final Renderer renderer;
    
    public final VectorPool vectorPool;
    public final ColorPool colorPool;
    public final Vector2DPool vector2DPool;
    public final PointPool pointPool;
    public final IntervalPool intervalPool;
    
    RenderingThread(final Renderer renderer, final Queue<WorkBlock> workQueue)
    {
        super("Rendering thread #" + (index++));
        
        this.renderer = renderer;
        this.workQueue = workQueue;
        
        this.vectorPool = new VectorPool();
        this.colorPool = new ColorPool();
        this.vector2DPool = new Vector2DPool();
        this.pointPool = new PointPool();
        this.intervalPool = new IntervalPool();
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
        pointPool.reset();
        intervalPool.reset();
    }
}
