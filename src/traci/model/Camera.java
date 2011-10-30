package traci.model;

import java.util.Random;

import traci.math.Transformable;
import traci.math.Transformation;
import traci.math.Transformations;
import traci.math.Vector;
import traci.render.Settings;

public class Camera implements Transformable
{
    private final double aspectRatio = 2560.0 / 1440.0;
    private final double fovx = (50 / 360.0) * Math.PI * 2.0;
    private final double fovy = fovx / aspectRatio;

    private Transformation transformation;
    public final double focalDist = 21;
    public final double aperture = .5;

    private final double xx;
    private final double yy;

    public Camera(final Vector location, final Vector lookAt, final Vector up)
    {
        transformation = Transformations.camera(location, lookAt, up);

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

        if (settings.focalBlurEnabled)
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
