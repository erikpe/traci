package traci.model.material.pigment;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import traci.model.material.Color;

public abstract class PngImage extends NonUniform implements Interpolatable
{
    protected enum Repeater { REPEAT, BORDER, STRETCH };
    
    private BufferedImage image;
    
    private final Interpolator interpolator = Interpolator.LINEAR;
    
    private final Repeater repeater;
    
    public PngImage(final String filename, final Repeater repeater)
    {
        this.repeater = repeater;
        readFile(filename);
    }
    
    private void readFile(final String filename)
    {
        final File file = new File(filename);
        
        try
        {
            image = ImageIO.read(file);
        }
        catch (final IOException e)
        {
            System.err.println(" *** ERROR: Failed to read file: " + filename);
            System.exit(-1);
        }
    }
    
    protected Color getSample(double x, double y)
    {
        switch(repeater)
        {
        case REPEAT:
            x = x - Math.floor(x);
            y = y - Math.floor(y);
            break;
            
        case BORDER:
            if (x < 0.0 || x > 1.0 || y < 0.0 || y > 1.0)
            {
                return Color.BLACK;
            }
            break;
            
        case STRETCH:
            x = Math.max(0.0, Math.min(1.0, x));
            y = Math.max(0.0, Math.min(1.0, y));
            break;
        }
        
        return interpolator.interpolate(this, x * (getWidth() - 1),
                y * (getHeight() - 1));
    }
    
    public long getWidth()
    {
        return image.getWidth();
    }
    
    public long getHeight()
    {
        return image.getHeight();
    }
    
    public Color getAt(final long x, final long y)
    {
        return Color.makeRGB(image.getRGB((int) x, (int) y));
    }
}
