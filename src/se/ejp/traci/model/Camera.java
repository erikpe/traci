package se.ejp.traci.model;

import java.util.Random;

import se.ejp.traci.main.options.Settings;
import se.ejp.traci.math.Transformable;
import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.math.Vector;

public class Camera implements Transformable, Cloneable
{
    private boolean initialized;

    private double aspectRatio;
    private double fovx;
    private double fovy;

    private Transformation transformation;
    public double focalDist = 45;
    public double aperture = .4;

    private double xx;
    private double yy;

    private Camera(final Vector location, final Vector lookAt, final Vector up)
    {
        transformation = Transformations.camera(location, lookAt, up);
        initialized = false;
    }

    public static Camera make(final Vector location, final Vector lookAt, final Vector up)
    {
        return new Camera(location, lookAt, up);
    }

    public static Camera make(final Vector location, final Vector lookAt)
    {
        return make(location, lookAt, Vector.UNIT_Y);
    }

    @Override
    public void transform(final Transformation tr)
    {
        transformation = transformation.compose(tr);
    }

    public Transformation getTransformation()
    {
        return transformation;
    }

    public void initialize(final Settings settings)
    {
        aspectRatio = ((double) settings.getWidth()) / settings.getHeight();
        fovx = (settings.getFov() / 360.0) * Math.PI * 2.0;
        fovy = fovx / aspectRatio;
        xx = 2.0 * Math.tan(fovx / 2.0);
        yy = 2.0 * Math.tan(fovy / 2.0);
        initialized = true;
    }

    public Vector[] getLocAndDir(final double lookX, final double lookY, final Settings settings, final Random randomSource)
    {
        if (!initialized)
        {
            throw new RuntimeException("Camera not initialized with settings yet");
        }

        final Vector[] res = new Vector[2];
        final Vector location;

        if (settings.getFocalBlurEnabled())
        {
            final double r = Math.sqrt(randomSource.nextDouble()) / 2.0;
            final double phi = randomSource.nextDouble() * Math.PI * 2.0;

            final double apertureX = r * Math.cos(phi);
            final double apertureY = r * Math.sin(phi);

            location = Vector.make(aperture * apertureX, aperture * apertureY, 0);
        }
        else
        {
            location = Vector.ORIGO;
        }

        final double x = (lookX - 0.5) * xx;
        final double y = (0.5 - lookY) * yy;

        final Vector lookAt = Vector.make(x, y, 1).mul(focalDist);
        final Vector dir = lookAt.sub(location).normalize();

        res[0] = transformation.point(location);
        res[1] = transformation.dir(dir);

        return res;
    }

    @Override
    public Camera clone() throws CloneNotSupportedException
    {
        return (Camera) super.clone();
    }
}
