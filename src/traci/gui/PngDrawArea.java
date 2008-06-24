package traci.gui;

import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

public class PngDrawArea extends BufferedImageDrawArea implements DrawArea
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
                throw new IOException("Can't write to file.");
            }
        }
        catch (final Exception e)
        {
            System.err.println(" *** ERROR: Failed to open file for writing: "
                    + outputFile);
            
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
            System.err.println(" *** ERROR: Failed to write image: "
                    + outputFile + ": " + e.getMessage());
            
            System.exit(-1);
        }
        
        System.out.println("> Wrote image to file: " + outputFile);
    }
}
