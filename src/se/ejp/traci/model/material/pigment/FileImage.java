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

public class FileImage extends NonUniform implements Interpolatable
{
    private static final Color DEFAULT_BORDER_COLOR = Color.BLACK;

    private static WeakCache<FileImage> cache = new WeakCache<FileImage>();
    private static Map<String, BufferedImage> imageCache = new HashMap<String, BufferedImage>();

    private final String filename;
    private final Interpolator interpolator = Interpolator.BI_LINEAR;
    private final RepeatPolicy repeatPolicy;
    private final Projection2D projection;
    private final Color borderColor;

    private final BufferedImage image;

    private FileImage(final String filename, final RepeatPolicy repeatPolicy, final Projection2D projection,
            final Color borderColor, final Transformation transformation)
    {
        super(transformation);

        this.repeatPolicy = repeatPolicy;
        this.projection = projection;
        this.borderColor = borderColor;
        this.filename = filename;

        this.image = getImage(filename);
    }

    public static FileImage make(final String filename, final String repeatPolicyStr, final String projStr,
            final Color borderColor)
    {
        final RepeatPolicy repeat = RepeatPolicy.get(repeatPolicyStr);

//        if (repeat == null)
//        {
//            throw new InterpreterRuntimeException(null, null, "Unknown repeat policy") { };
//        }
//
        final Projection2D proj = Projection2D.get(projStr);
//
//        if (proj == null)
//        {
//            throw new InterpreterRuntimeException(null, null, "Unknown projection") { };
//        }

        return cache.get(new FileImage(filename, repeat, proj, borderColor, Transformations.identity()));
    }

    public static FileImage make(final String filename, final String repeatPolicyStr, final String projStr)
    {
        return make(filename, repeatPolicyStr, projStr, DEFAULT_BORDER_COLOR);
    }

    @Override
    public FileImage transform(final Transformation newTr)
    {
        return cache.get(new FileImage(filename, repeatPolicy, projection, borderColor, transformation.compose(newTr)));
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

        final FileImage otherFileImage = (FileImage) other;

        return transformation.equals(otherFileImage.transformation) &&
               interpolator.equals(otherFileImage.interpolator) &&
               repeatPolicy.equals(otherFileImage.repeatPolicy) &&
               projection.equals(otherFileImage.projection) &&
               borderColor.equals(otherFileImage.borderColor);
    }
}
