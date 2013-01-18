package traci.model;

import java.util.Random;

import traci.main.options.Settings;
import traci.math.Transformable;
import traci.math.Transformation;
import traci.math.Transformations;
import traci.math.Vector;

public class Camera implements Transformable
{
    private final double aspectRatio;
    private final double fovx;
    private final double fovy;

    private Transformation transformation;
    public final double focalDist = 21;
    public final double aperture = .5;

    private final double xx;
    private final double yy;

    public Camera(final Vector location, final Vector lookAt, final Vector up, final Settings settings)
    {
        this.transformation = Transformations.camera(location, lookAt, up);

        this.aspectRatio = ((double) settings.getWidth()) / settings.getHeight();
        this.fovx = (settings.getFov() / 360.0) * Math.PI * 2.0;
        this.fovy = fovx / aspectRatio;
        this.xx = 2.0 * Math.tan(fovx / 2.0);
        this.yy = 2.0 * Math.tan(fovy / 2.0);
    }

    @Override
    public void transform(final Transformation tr)
    {
        transformation = transformation.compose(tr);
    }

    public Vector[] getLocAndDir(final double lookX, final double lookY, final Settings settings, final Random randomSource)
    {
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
}
