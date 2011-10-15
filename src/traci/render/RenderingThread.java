package traci.render;

import java.util.Queue;
import java.util.Random;

import traci.math.Vector.VectorPool;
import traci.math.Vector2D.Vector2DPool;
import traci.model.material.Color.ColorPool;
import traci.render.Point2.Point2Pool;
import traci.render.Ray2.Ray2Pool;

public class RenderingThread extends Thread
{
    static class WorkBlock
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
    
    static interface BlockRenderer
    {
        public void renderBlock(final WorkBlock block);
    }
    
    private static long index = 0;
    
    private final Queue<WorkBlock> workQueue;
    private final BlockRenderer renderer;
    
    public final VectorPool vectorPool;
    public final ColorPool colorPool;
    public final Vector2DPool vector2DPool;
    public final Point2Pool point2Pool;
    public final Ray2Pool ray2Pool;
    
    RenderingThread(final BlockRenderer renderer,
            final Queue<WorkBlock> workQueue)
    {
        super("Rendering thread #" + nextIndex());
        
        this.renderer = renderer;
        this.workQueue = workQueue;
        
        this.vectorPool = new VectorPool();
        this.colorPool = new ColorPool();
        this.vector2DPool = new Vector2DPool();
        this.point2Pool = new Point2Pool();
        this.ray2Pool = new Ray2Pool();
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
        point2Pool.reset();
        ray2Pool.reset();
    }
    
    private static synchronized long nextIndex()
    {
        return index++;
    }
}
