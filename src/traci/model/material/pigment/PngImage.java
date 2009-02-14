package traci.model.material.pigment;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.imageio.ImageIO;

import traci.math.Projection2D;
import traci.math.Vector;
import traci.math.Vector2D;
import traci.model.material.Color;

public class PngImage extends NonUniform implements Interpolatable
{
    public enum RepeatPolicy { REPEAT, BORDER, STRETCH }
    
    private static Map<File, BufferedImage> imageCache =
        new HashMap<File, BufferedImage>();
    
    private final BufferedImage image;
    
    private final Interpolator interpolator = Interpolator.BI_LINEAR;
    
    private final RepeatPolicy repeatPolicy;
    
    private final Projection2D projection;
    
    private final Color borderColor;
    
    public PngImage(final String filename, final RepeatPolicy repeater,
            final Projection2D projection)
    {
        this(filename, repeater, projection, Color.BLACK);
    }
    
    public PngImage(final String filename, final RepeatPolicy repeater,
            final Projection2D projection, final Color borderColor)
    {
        this.repeatPolicy = repeater;
        this.projection = projection;
        this.borderColor = borderColor;
        this.image = readFile(filename);
    }
    
    private BufferedImage readFile(final String filename)
    {
        final File file = new File(filename);
        
        BufferedImage image = imageCache.get(file);
        
        if (image == null)
        {
            try
            {
                image = ImageIO.read(file);
            }
            catch (final IOException e)
            {
                System.err.println(" *** ERROR: Failed to read file: " + filename);
                System.exit(-1);
            }
            
            imageCache.put(file, image);
        }
        
        return image;
    }
    
    @Override
    protected Color getColorTransformed(final Vector p)
    {
        final Vector2D projected = projection.project(p);
        
        double x = projected.x;
        double y = projected.y;
        
        switch(repeatPolicy)
        {
        case REPEAT:
            x = x - Math.floor(x);
            y = y - Math.floor(y);
            break;
            
        case BORDER:
            if (x < 0.0 || x > 1.0 || y < 0.0 || y > 1.0)
            {
                return borderColor;
            }
            break;
            
        case STRETCH:
            x = Math.max(0.0, Math.min(1.0, x));
            y = Math.max(0.0, Math.min(1.0, y));
            break;
        }
        
        y = 1.0 - y;
        
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
    
    @Override
    public Color getAt(final long x, final long y)
    {
        return Color.makeRGB(image.getRGB((int) x, (int) y));
    }
}
