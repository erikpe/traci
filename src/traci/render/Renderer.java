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
import traci.model.shape.BoundingBox;
import traci.model.shape.ShapeHelper;
import traci.render.RenderingThread.BlockRenderer;
import traci.render.RenderingThread.WorkBlock;

public class Renderer implements BlockRenderer
{
    private final Scene scene;
    private final Camera camera;
    private final Settings settings;
    private final DrawArea area;
    private final int numThreads;

    private Renderer(final Scene scene, final Settings settings, final DrawArea area, final int numThreads)
    {
        this.scene = scene;
        this.camera = scene.camera;
        this.settings = settings;
        this.area = area;
        this.numThreads = numThreads;
    }

    public static void renderScene(final Scene scene, final Settings settings, final DrawArea area, final int numThreads)
    {
        new Renderer(scene, settings, area, numThreads).renderScene();
    }

    private void renderScene()
    {
        System.out.println("> Rendering scene consisting of "
                + ShapeHelper.numPrimitives(scene.shape)
                + " primitive objects, " + ShapeHelper.numCsgs(scene.shape)
                + " CSG objects and " + ShapeHelper.numBBoxes(scene.shape)
                + " bounding boxes");

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

                workQueue.add(new WorkBlock(x, y, width, height, new Random(seedSource.nextLong())));
            }
        }

        /**
         * Create all rendering threads
         */
        final List<RenderingThread> renderingThreads = new ArrayList<RenderingThread>();

        for (int i = 0; i < numThreads; ++i)
        {
            renderingThreads.add(new RenderingThread(this, workQueue));
        }

        System.out.println("> Spawning " + numThreads + " rendering thread" + (numThreads == 1 ? "" : "s")
                + " working on " + workQueue.size() + " blocks ...");

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

        final long hit = BoundingBox.hit.get();
        final long miss = BoundingBox.miss.get();

        System.out.println("> Bounding-Box discard ratio: "
                + ((100 * miss) / (miss + hit)) + "% (" + miss + "/" + (miss + hit) + ")");
    }

    @Override
    public void renderBlock(final WorkBlock block)
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
        Color color = Color.BLACK;

        for (int aay = -settings.aaLevel; aay <= settings.aaLevel; ++aay)
        {
            for (int aax = -settings.aaLevel; aax <= settings.aaLevel; ++aax)
            {
                for (int i = 0; i < settings.focalBlurSamples; ++i)
                {
                    final double subX = aax / (settings.aaLevel * 2.0 + 1);
                    final double subY = aay / (settings.aaLevel * 2.0 + 1);

                    final double lookX = (x + subX) / (area.width() - 1); // [0.0 .. 1.0]
                    final double lookY = (y + subY) / (area.height() - 1); // [0.0 .. 1.0]

                    final Vector[] cam = camera.getLocAndDir(lookX, lookY, settings, block.randomSource);
                    final Color rayColor = Raytrace.raytrace(scene, 5, cam[0], cam[1]);
                    color = color.add(rayColor);
                }
            }
        }

        area.draw(x, y, color.div(settings.focalBlurSamples * (settings.aaLevel * 2 + 1) * (settings.aaLevel * 2 + 1)));
    }
}
