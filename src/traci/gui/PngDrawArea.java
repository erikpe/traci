package traci.gui;

import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import traci.main.Result;
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
    public Result start()
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
                Log.ERROR("Unable to open file for writing: " + outputFile);
                System.exit(-1);
            }
        }
        catch (final IOException e)
        {
            Log.ERROR("Unable to open file for writing: " + outputFile + "\n" + e.getMessage());
            return Result.IO_ERROR;
        }
        catch (final SecurityException e)
        {
            Log.ERROR("Unable to open file for writing: " + outputFile + "\n" + e.getMessage());
            return Result.IO_ERROR;
        }

        super.start();

        return Result.SUCCESS;
    }

    @Override
    public Result finish()
    {
        super.finish();

        try
        {
            ImageIO.write(getBufferedImage(), "png", outputFile);
        }
        catch (final IOException e)
        {
            Log.ERROR("Unable to write image file: " + outputFile + "\n" + e.getMessage());
            return Result.IO_ERROR;
        }
        catch (final SecurityException e)
        {
            Log.ERROR("Unable to write image file: " + outputFile + "\n" + e.getMessage());
            return Result.IO_ERROR;
        }

        Log.INFO("Wrote output image to file: " + outputFile);

        return Result.SUCCESS;
    }
}
