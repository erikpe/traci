package se.ejp.traci.model.material.pigment;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.imageio.ImageIO;

import se.ejp.traci.math.Projection2D;
import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.math.Vector;
import se.ejp.traci.math.Vector2D;
import se.ejp.traci.model.material.Color;
import se.ejp.traci.util.WeakCache;

public class PngImage extends NonUniform implements Interpolatable
{
    public enum RepeatPolicy { REPEAT, BORDER, STRETCH }

    private static WeakCache<PngImage> cache = new WeakCache<PngImage>();
    private static Map<String, BufferedImage> imageCache = new HashMap<String, BufferedImage>();

    private final String filename;
    private final Interpolator interpolator = Interpolator.BI_LINEAR;
    private final RepeatPolicy repeatPolicy;
    private final Projection2D projection;
    private final Color borderColor;

    private final BufferedImage image;

    private PngImage(final String filename, final RepeatPolicy repeater, final Projection2D projection,
            final Color borderColor, final Transformation transformation)
    {
        super(transformation);

        this.repeatPolicy = repeater;
        this.projection = projection;
        this.borderColor = borderColor;
        this.filename = filename;

        this.image = getImage(filename);
    }

    public static PngImage make(final String filename, final String repeatPolicyStr, final String projStr)
    {
        return cache.get(new PngImage(filename, RepeatPolicy.REPEAT, Projection2D.CYLINDER, Color.BLACK,
                Transformations.identity()));
    }

    @Override
    public PngImage transform(final Transformation newTr)
    {
        return cache.get(new PngImage(filename, RepeatPolicy.REPEAT, Projection2D.CYLINDER, Color.BLACK, transformation
                .compose(newTr)));
    }

    private static BufferedImage getImage(final String filename)
    {
        BufferedImage tmpImage = imageCache.get(filename);

        if (tmpImage == null)
        {
            final File file = new File(filename);

            try
            {
                tmpImage = ImageIO.read(file);
            }
            catch (final IOException e)
            {
                System.err.println(" *** ERROR: Failed to read file: " + filename);
                System.exit(-1);
            }

            imageCache.put(filename, tmpImage);
        }

        return tmpImage;
    }

    @Override
    public Color getColorTransformed(final Vector p)
    {
        final Vector2D projected = projection.project(p);

        double x = projected.x();
        double y = projected.y();

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

        return interpolator.interpolate(this, x * (getWidth() - 1), y * (getHeight() - 1));
    }

    @Override
    public long getWidth()
    {
        return image.getWidth();
    }

    @Override
    public long getHeight()
    {
        return image.getHeight();
    }

    @Override
    public Color getAt(final long x, final long y)
    {
        return Color.makeRGB(image.getRGB((int) x, (int) y));
    }

    @Override
    public int hashCode()
    {
        int hash = getClass().hashCode();
        hash = 31 * hash + transformation.hashCode();
        hash = 31 * hash + interpolator.hashCode();
        hash = 31 * hash + repeatPolicy.hashCode();
        hash = 31 * hash + projection.hashCode();
        hash = 31 * hash + borderColor.hashCode();
        return hash;
    }

    @Override
    public boolean equals(final Object other)
    {
        if (other == null)
        {
            return false;
        }
        else if (other == this)
        {
            return true;
        }
        else if (other.getClass() != getClass())
        {
            return false;
        }

        final PngImage otherPngImage = (PngImage) other;

        return transformation.equals(otherPngImage.transformation) &&
               interpolator.equals(otherPngImage.interpolator) &&
               repeatPolicy.equals(otherPngImage.repeatPolicy) &&
               projection.equals(otherPngImage.projection) &&
               borderColor.equals(otherPngImage.borderColor);
    }
}
