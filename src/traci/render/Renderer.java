package traci.render;

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

import traci.gui.DrawArea;
import traci.math.Vector;
import traci.model.Camera;
import traci.model.Scene;
import traci.model.material.Color;

public class Renderer
{
    private static int WORK_BLOCK_WIDTH = 100;
    private static int WORK_BLOCK_HEIGHT = 100;
    
    public static void renderScene(final Scene scene, final DrawArea area,
            final int numThreads)
    {
        /**
         * Divide the area into work blocks and put them on a work queue
         */
        final Queue<Rectangle> workQueue = new ConcurrentLinkedQueue<Rectangle>();
        for (int y = 0; y < area.height(); y += WORK_BLOCK_HEIGHT)
        {
            for (int x = 0; x < area.width(); x += WORK_BLOCK_WIDTH)
            {
                final int width = Math.min(WORK_BLOCK_WIDTH, area.width() - x);
                final int height = Math.min(WORK_BLOCK_HEIGHT, area.height() - y);
                
                workQueue.add(new Rectangle(x, y, width, height));
            }
        }
        
        /**
         * Create all rendering threads
         */
        final List<Thread> renderThreads = new ArrayList<Thread>();
        for (int i = 0; i < numThreads; ++i)
        {
            renderThreads.add(new Thread("Rendering thread #" + i)
            {
                @Override
                public void run()
                {
                    Rectangle block;
                    
                    while ((block = workQueue.poll()) != null)
                    {
                        renderBlock(scene, area, block);
                    }
                }
            });
        }
        
        System.out.println("> Spawning " + numThreads
                + " rendering thread" + (numThreads == 1 ? "" : "s")
                + " working on " + workQueue.size()
                + " blocks ...");
        
        /**
         * Start the threads
         */
        area.start();
        final long startTime = System.currentTimeMillis();
        if (numThreads == 1)
        {
            renderThreads.get(0).run();
        }
        else
        {
            for (final Thread thread : renderThreads)
            {
                thread.start();
            }
        }
        
        /**
         * Wait for all threads to finish
         */
        if (numThreads != 1)
        {
            for (final Thread thread : renderThreads)
            {
                try
                {
                    thread.join();
                }
                catch (final InterruptedException e)
                {
                    e.printStackTrace();
                }
            }
        }
        
        final long stopTime = System.currentTimeMillis();
        area.finish();
        
        System.out.println("> Successfully rendered scene in "
                + ((stopTime - startTime) / 1000.0)
                + " seconds.");
    }
    
    private static void renderBlock(final Scene scene, final DrawArea area,
            final Rectangle block)
    {
        final Camera cam = scene.camera;
        
        final double fovy = cam.fov * (((double) area.height()) / area.width());
        final double xx = 2.0 * Math.tan(cam.fov / 2.0);
        final double yy = 2.0 * Math.tan(fovy / 2.0);
        
        for (int y = block.y; y < block.y + block.height; ++y)
        {
            for (int x = block.x; x < block.x + block.width; ++x)
            {
                Color color = Color.BLACK;
                
                double lookX = ((double) x) / (area.width()-1); // [0.0 .. 1.0]
                double lookY = ((double) y) / (area.height()-1); // [0.0 .. 1.0]
                
                lookX = (lookX - 0.5); // [-0.5 .. 0.5]
                lookY = (0.5 - lookY); // [0.5 .. -0.5] 
                
                lookX = lookX * xx;
                lookY = lookY * yy;
                
                final Vector dir = Vector.make(lookX, lookY, -1.0);
                final Vector camDir = cam.mat.mul(dir).normalize();
                
                color = color.add(Raytrace.raytrace(scene, 5, cam.location, camDir));
                
                area.draw(x, y, color);
            }
        }
    }
}
