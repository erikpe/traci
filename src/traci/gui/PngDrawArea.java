package traci.gui;

import java.io.File;

import javax.imageio.ImageIO;

import traci.util.Log;

public class PngDrawArea extends BufferedImageDrawArea
{
    private final String filename;
    private File outputFile = null;

    public PngDrawArea(final int width, final int height, final String filename)
    {
        super(width, height);
        this.filename = filename;
    }

    @Override
    public void start()
    {
        assert outputFile == null;
        outputFile = new File(filename);

        try
        {
            if (!outputFile.exists())
            {
                outputFile.createNewFile();
            }

            if (!outputFile.canWrite())
            {
                Log.ERROR("Unable to write to file: " + outputFile);
                System.exit(-1);
            }
        }
        catch (final Exception e)
        {
            Log.ERROR("Failed to open file for writing: " + outputFile);
            Log.ERROR(e.getMessage());
            System.exit(-1);
        }

        super.start();
    }

    @Override
    public void finish()
    {
        super.finish();

        try
        {
            ImageIO.write(getBufferedImage(), "png", outputFile);
        }
        catch (final Exception e)
        {
            Log.ERROR("Failed to write image file: " + outputFile);
            Log.ERROR(e.getMessage());
            System.exit(-1);
        }

        Log.INFO("Wrote output image to file: " + outputFile);
    }
}
