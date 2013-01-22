package traci.model.shape.primitive;

import traci.math.Vector;
import traci.render.Point.Type;
import traci.render.Ray;

public class Sphere extends Primitive
{
    @Override
    public Vector primitiveGetNormalAt(final Vector p)
    {
        return p;
    }

    /**
     * The sphere is define by the equation
     *
     * x^2 + y^2 + z^2 = 1
     */
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        final double c = dir.dot(dir);
        final double a = 2 * p.dot(dir) / c;
        final double b = (p.dot(p) - 1) / c;

        final double d = (a * a) / 4 - b;

        if (d > 0)
        {
            final double sqrtD = Math.sqrt(d);
            final double ma2 = -a / 2;

            final double t0 = ma2 - sqrtD;
            final double t1 = ma2 + sqrtD;

            if (t1 > -EPSILON && t0 < t1)
            {
                final Ray ray = Ray.make();

                ray.add(t0, this, Type.ENTER);
                ray.add(t1, this, Type.LEAVE);

                return ray;
            }
        }

        return null;
    }

    @Override
    public String toString()
    {
        return "Sphere";
    }
}
