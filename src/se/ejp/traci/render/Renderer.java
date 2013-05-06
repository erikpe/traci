package se.ejp.traci.render;

import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.Random;
import java.util.concurrent.ConcurrentLinkedQueue;

import se.ejp.traci.gui.DrawArea;
import se.ejp.traci.main.Result;
import se.ejp.traci.main.options.Settings;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.Camera;
import se.ejp.traci.model.Scene;
import se.ejp.traci.model.material.Color;
import se.ejp.traci.model.shape.BoundingBox;
import se.ejp.traci.model.shape.ShapeHelper;
import se.ejp.traci.render.RenderingThread.BlockRenderer;
import se.ejp.traci.render.RenderingThread.WorkBlock;
import se.ejp.traci.util.Log;
import se.ejp.traci.util.Utilities;

public class Renderer implements BlockRenderer
{
    private final Scene scene;
    private final Camera camera;
    private final Settings settings;
    private final DrawArea area;

    private Renderer(final Scene scene, final Settings settings, final DrawArea area)
    {
        this.scene = scene;
        this.camera = scene.camera;
        this.settings = settings;
        this.area = area;
    }

    public static Result renderScene(final Scene scene, final Settings settings, final DrawArea area)
    {
        return new Renderer(scene, settings, area).renderScene();
    }

    private Result renderScene()
    {
        final int numPrim = ShapeHelper.numPrimitives(scene.rootShape);
        final int numCsg = ShapeHelper.numCsgs(scene.rootShape);
        final int numBBox = ShapeHelper.numBBoxes(scene.rootShape);

        Log.INFO("Number of primitive objects: " + numPrim);
        Log.INFO("Number of csg objects: " + numCsg);
        Log.INFO("Number of bounding boxes: " + numBBox);

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
        for (int y = 0; y < area.height(); y += settings.getWorkBlockHeight())
        {
            for (int x = 0; x < area.width(); x += settings.getWorkBlockWidth())
            {
                final int width = Math.min(settings.getWorkBlockWidth(), area.width() - x);
                final int height = Math.min(settings.getWorkBlockHeight(), area.height() - y);

                workQueue.add(new WorkBlock(x, y, width, height, new Random(seedSource.nextLong())));
            }
        }

        /**
         * Create all rendering threads
         */
        final List<RenderingThread> renderingThreads = new ArrayList<RenderingThread>();
        final int numThreads = settings.getNumThreads();

        for (int i = 0; i < numThreads; ++i)
        {
            renderingThreads.add(new RenderingThread(this, workQueue));
        }

        Log.INFO("Spawning " + numThreads + " rendering thread" + (numThreads == 1 ? "" : "s") + " wokning on " +
                workQueue.size() + " block");

        /**
         * Start the threads
         */
        Result result = area.start();
        if (result != Result.SUCCESS)
        {
            return result;
        }
        final long start = System.currentTimeMillis();
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

        final long stop = System.currentTimeMillis();
        result = area.finish();
        if (result != Result.SUCCESS)
        {
            return result;
        }

        Log.INFO("Successfully rendered scene in " + Utilities.millisecondsToString(stop - start));

        if (settings.getDebug())
        {
            final long hit = BoundingBox.hit.get();
            final long miss = BoundingBox.miss.get();
            Log.INFO("Bounding-Box discard ratio: " + ((100 * miss) / (miss + hit)) + "% (" + miss + "/" + (miss + hit) + ")");
        }

        return Result.SUCCESS;
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
        final int aaLevel = settings.getAaLevel();
        final int focalBlurSamples;
        if (settings.getFocalBlurEnabled())
        {
            focalBlurSamples = settings.getFocalBlurSamples();
        }
        else
        {
            focalBlurSamples = 1;
        }

        Color color = Color.BLACK;

        for (int aay = -aaLevel; aay <= aaLevel; ++aay)
        {
            for (int aax = -aaLevel; aax <= aaLevel; ++aax)
            {
                for (int i = 0; i < focalBlurSamples; ++i)
                {
                    final double subX = aax / (aaLevel * 2.0 + 1);
                    final double subY = aay / (aaLevel * 2.0 + 1);

                    final double lookX = (x + subX) / (area.width() - 1); // [0.0 .. 1.0]
                    final double lookY = (y + subY) / (area.height() - 1); // [0.0 .. 1.0]

                    final Vector[] cam = camera.getLocAndDir(lookX, lookY, settings, block.randomSource);
                    final Color rayColor = Raytrace.raytrace(scene, 5, cam[0], cam[1]);
                    color = color.add(rayColor);
                }
            }
        }

        area.draw(x, y, color.div(focalBlurSamples * (aaLevel * 2 + 1) * (aaLevel * 2 + 1)));
    }
}
