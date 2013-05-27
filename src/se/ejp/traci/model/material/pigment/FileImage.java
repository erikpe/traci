package se.ejp.traci.model.material.pigment;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.imageio.ImageIO;

import se.ejp.traci.lang.interpreter.exceptions.InterpreterIOException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.math.Projection2D;
import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.math.Vector;
import se.ejp.traci.math.Vector2D;
import se.ejp.traci.model.Color;
import se.ejp.traci.util.WeakCache;

public class FileImage extends NonUniform implements Interpolatable
{
    private static Map<String, BufferedImage> imageCache = new HashMap<String, BufferedImage>();
    private static WeakCache<FileImage> cache = new WeakCache<FileImage>();
    private final int hash;

    private final String filename;
    private final Interpolator interpolator = Interpolator.BI_LINEAR;
    private final RepeatPolicy repeatPolicy;
    private final Projection2D projection;
    private final Color borderColor;

    private final BufferedImage image;

    private FileImage(final String filename, final RepeatPolicy repeatPolicy, final Projection2D projection,
            final Color borderColor, final Transformation transformation, final BufferedImage image)
    {
        super(transformation);

        this.filename = filename;
        this.repeatPolicy = repeatPolicy;
        this.projection = projection;
        this.borderColor = borderColor;

        this.image = image;

        this.hash = calcHash();
    }

    public static FileImage make(final String filename, final String repeatPolicyStr, final String projStr,
            final Color borderColor) throws InterpreterRuntimeException
    {
        final BufferedImage image = getCachedImage(filename);
        final RepeatPolicy repeatPolicy = getRepeatPolicy(repeatPolicyStr);
        final Projection2D projection = getProjection(projStr);
        final Transformation eye = Transformations.identity();

        return cache.get(new FileImage(filename, repeatPolicy, projection, borderColor, eye, image));
    }

    public static FileImage make(final String filename, final String repeatPolicyStr, final String projStr)
            throws InterpreterRuntimeException
    {
        return make(filename, repeatPolicyStr, projStr, Color.BLACK);
    }

    @Override
    public FileImage transform(final Transformation tr)
    {
        final Transformation newTr = transformation.compose(tr);
        return cache.get(new FileImage(filename, repeatPolicy, projection, borderColor, newTr, image));
    }

    private static BufferedImage getCachedImage(final String filename) throws InterpreterIOException
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
                throw new InterpreterIOException(null, null, "Unable to read file: '" + filename + "'", e);
            }

            imageCache.put(filename, tmpImage);
        }

        return tmpImage;
    }

    private static RepeatPolicy getRepeatPolicy(final String repeatPolicyStr) throws InterpreterRuntimeException
    {
        final RepeatPolicy repeatPolicy = RepeatPolicy.get(repeatPolicyStr);

        if (repeatPolicy != null)
        {
            return repeatPolicy;
        }

        final StringBuilder sb = new StringBuilder();
        sb.append("Unable to create 'image':").append('\n');
        sb.append("Argument 2: Unknown repeat policy: \"").append(repeatPolicyStr).append("\"").append('\n');
        sb.append("Must be one of following: [");

        String delim = "";
        for (final String repID : RepeatPolicy.getAllPolicies())
        {
            sb.append(delim).append("\"").append(repID).append("\"");
            delim = ", ";
        }

        sb.append(']');

        throw new InterpreterRuntimeException(null, null, sb.toString());
    }

    private static Projection2D getProjection(final String projStr) throws InterpreterRuntimeException
    {
        final Projection2D projection = Projection2D.get(projStr);

        if (projection != null)
        {
            return projection;
        }

        final StringBuilder sb = new StringBuilder();
        sb.append("Unable to create 'image':").append('\n');
        sb.append("Argument 3: Unknown projection type: \"").append(projStr).append("\"").append('\n');
        sb.append("Must be one of following: [");

        String delim = "";
        for (final String repID : Projection2D.getAllProjections())
        {
            sb.append(delim).append("\"").append(repID).append("\"");
            delim = ", ";
        }

        sb.append(']');

        throw new InterpreterRuntimeException(null, null, sb.toString());
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
        return Color.fromRGB(image.getRGB((int) x, (int) y));
    }

    @Override
    public int hashCode()
    {
        return hash;
    }

    private int calcHash()
    {
        int hash = getClass().hashCode();
        hash = 31 * hash + transformation.hashCode();
        hash = 31 * hash + filename.hashCode();
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
        else if (getClass() != other.getClass())
        {
            return false;
        }
        else if (hashCode() != other.hashCode())
        {
            return false;
        }

        final FileImage otherFileImage = (FileImage) other;

        return transformation.equals(otherFileImage.transformation) &&
               filename.equals(otherFileImage.filename) &&
               interpolator.equals(otherFileImage.interpolator) &&
               repeatPolicy.equals(otherFileImage.repeatPolicy) &&
               projection.equals(otherFileImage.projection) &&
               borderColor.equals(otherFileImage.borderColor);
    }
}
