package traci.render;

import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.Random;
import java.util.concurrent.ConcurrentLinkedQueue;

import traci.gui.DrawArea;
import traci.math.Vector;
import traci.model.Camera;
import traci.model.Scene;
import traci.model.material.Color;
import traci.render.RenderingThread.WorkBlock;

public class Renderer
{
    private final Scene scene;
    private final Camera camera;
    private final Settings settings;
    private final DrawArea area;
    private final int numThreads;
    
    private final double xx;
    private final double yy;
    
    private Renderer(final Scene scene, final Settings settings,
            final DrawArea area, final int numThreads)
    {
        this.scene = scene;
        this.camera = scene.camera;
        this.settings = settings;
        this.area = area;
        this.numThreads = numThreads;
        
        final double fovy = camera.fov * (((double) area.height()) / area.width());
        this.xx = 2.0 * Math.tan(camera.fov / 2.0);
        this.yy = 2.0 * Math.tan(fovy / 2.0);
    }
    
    public static void renderScene(final Scene scene, final Settings settings,
            final DrawArea area, final int numThreads)
    {
        new Renderer(scene, settings, area, numThreads).renderScene();
    }
    
    private void renderScene()
    {
        /**
         * To get determinism, each {@link WorkBlock} gets its own
         * {@link Random} source, each block with a deterministic seed. In this
         * way, the blocks can be rendered in any order with a completely
         * deterministic result.
         */
        final Random seedSource = new Random(0);
        
        /**
         * Divide the area into work blocks and put them on a work queue
         */
        final Queue<WorkBlock> workQueue = new ConcurrentLinkedQueue<WorkBlock>();
        for (int y = 0; y < area.height(); y += settings.workBlockHeight)
        {
            for (int x = 0; x < area.width(); x += settings.workBlockWidth)
            {
                final int width = Math.min(settings.workBlockWidth, area.width() - x);
                final int height = Math.min(settings.workBlockHeight, area.height() - y);
                
                workQueue.add(new WorkBlock(x, y, width, height, new Random(
                        seedSource.nextLong())));
            }
        }
        
        /**
         * Create all rendering threads
         */
        final List<RenderingThread> renderingThreads =
                new ArrayList<RenderingThread>();
        for (int i = 0; i < numThreads; ++i)
        {
            renderingThreads.add(new RenderingThread(this, workQueue));
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
        for (final Thread thread : renderingThreads)
        {
            thread.start();
        }
        
        /**
         * Wait for all threads to finish
         */
        for (final Thread thread : renderingThreads)
        {
            try
            {
                thread.join();
            }
            catch (final InterruptedException e)
            {
                e.printStackTrace();
                System.exit(-1);
            }
        }
        
        final long stopTime = System.currentTimeMillis();
        area.finish();
        
        System.out.println("> Successfully rendered scene in "
                + ((stopTime - startTime) / 1000.0)
                + " seconds.");
    }
    
    protected void renderBlock(final WorkBlock block)
    {
        for (long y = block.y; y < block.y + block.height; ++y)
        {
            for (long x = block.x; x < block.x + block.width; ++x)
            {
                renderPixel(x, y, block);
            }
        }
    }
    
    private void renderPixel(final long x, final long y, final WorkBlock block)
    {
        final Color color = Color.makeCopy(Color.BLACK);
        
        for (int aay = -settings.aaLevel; aay <= settings.aaLevel; ++aay)
        {
            for (int aax = -settings.aaLevel; aax <= settings.aaLevel; ++aax)
            {
                for (int i = 0; i < settings.focalBlurSamples; ++i)
                {
                    final Thread thisThread = Thread.currentThread();
                    
                    if (thisThread instanceof RenderingThread)
                    {
                        ((RenderingThread) thisThread).resetPools();
                    }
                    
                    final double subX = aax / (settings.aaLevel * 2.0 + 1);
                    final double subY = aay / (settings.aaLevel * 2.0 + 1);
                    
                    double lookX = (x + subX) / (area.width() - 1); // [0.0 .. 1.0]
                    double lookY = (y + subY) / (area.height() - 1); // [0.0 .. 1.0]
                    
                    lookX = (lookX - 0.5); // [-0.5 .. 0.5]
                    lookY = (0.5 - lookY); // [0.5 .. -0.5]
                    
                    lookX = lookX * xx;
                    lookY = lookY * yy;
                    
                    final double r = Math.sqrt(block.randomSource.nextDouble()) / 2.0;
                    final double phi = block.randomSource.nextDouble() * Math.PI * 2.0;
                    
                    final double apertureX = r * Math.cos(phi);
                    final double apertureY = r * Math.sin(phi);
                    
                    final Vector lookAt = Vector.make(lookX, lookY, -1.0).mul(
                            camera.focalDist);
                    final Vector location = Vector.make(camera.aperture * apertureX,
                            camera.aperture * apertureY, 0);
                    
                    final Vector camLoc = camera.transformation.point(location);
                    final Vector camDir = camera.transformation.dir(
                            lookAt.sub(location)).normalize();
                    
                    final Color rayColor = Raytrace.raytrace(scene, 5, camLoc, camDir);
                    
                    color.r += rayColor.r;
                    color.g += rayColor.g;
                    color.b += rayColor.b;
                }
            }
        }
        
        area.draw(x, y, color.div(settings.focalBlurSamples
                * (settings.aaLevel * 2 + 1) * (settings.aaLevel * 2 + 1)));
    }
}
