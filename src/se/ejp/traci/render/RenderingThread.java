package se.ejp.traci.render;

import java.util.Queue;
import java.util.Random;

public class RenderingThread extends Thread
{
    static class WorkBlock
    {
        public final long x;
        public final long y;
        public final long width;
        public final long height;

        public final Random randomSource;

        WorkBlock(final long x, final long y, final long width, final long height, final Random randomSource)
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

    RenderingThread(final BlockRenderer renderer, final Queue<WorkBlock> workQueue)
    {
        super("Rendering thread #" + nextIndex());

        this.renderer = renderer;
        this.workQueue = workQueue;
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

    private static synchronized long nextIndex()
    {
        return index++;
    }
}