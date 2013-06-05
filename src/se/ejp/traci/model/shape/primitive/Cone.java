package se.ejp.traci.model.shape.primitive;

import se.ejp.traci.math.Transformations;
import se.ejp.traci.math.Vector;
import se.ejp.traci.render.Point.Type;
import se.ejp.traci.render.Ray;

public class Cone extends Primitive
{
    private final double k, lower, upper;

    private Cone(final double k, final double lower, final double upper)
    {
        this.k = k;
        this.lower = lower;
        this.upper = upper;
    }

    public static Cone make(Vector v0, Double r0, Vector v1, Double r1)
    {
        if (r0 > r1)
        {
            final Vector vTmp = v0;
            v0 = v1;
            v1 = vTmp;

            final Double rTmp = r0;
            r0 = r1;
            r1 = rTmp;
        }

        final double length = v1.sub(v0).length();
        final double lower = r0 * (length / (r1 - r0));
        final double upper = lower + length;
        final double k = r1 / upper;

        final Cone cone = new Cone(k, lower, upper);

        cone.transform(Transformations.translate(0.0, -lower, 0.0));
        cone.transform(Transformations.rotVecToVec(Vector.UNIT_Y, v1.sub(v0)));
        cone.transform(Transformations.translate(v0));

        return cone;
    }

    @Override
    protected Vector primitiveGetNormalAt(final Vector p)
    {
        final double x = p.x();
        final double y = p.y();
        final double z = p.z();

        double up = -k * Math.sqrt(x * x + z * z);
        if (y < 0.0)
        {
            up = -up;
        }

        return Vector.make(x, 0, z);
    }

    @Override
    protected Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        final double px = p.x();
        final double py = p.y();
        final double pz = p.z();

        final double dirx = dir.x();
        final double diry = dir.y();
        final double dirz = dir.z();

        final double y0 = (lower - py) / diry;
        final double y1 = (upper - py) / diry;

        double near = min(y0, y1);
        double far = max(y0, y1);

        final double g = dirx * dirx + dirz * dirz - k * k * diry * diry;
        final double a = (2 * (px * dirx + pz * dirz - k * k * py * diry)) / g;
        final double b = (px * px + pz * pz - k * k * py * py) / g;

        final double d = (a * a) / 4.0 - b;
        if (d > 0.0)
        {
            final double ma2 = -a / 2;
            final double root = Math.sqrt(d);

            final double t0 = ma2 - root;
            final double t1 = ma2 + root;

            near = max(near, t0);
            far = min(far, t1);

            if (far > -EPSILON && near < far)
            {
                final Ray ray = Ray.make();

                ray.add(t0, this, Type.ENTER);
                ray.add(t1, this, Type.LEAVE);

                return ray;
            }
        }

        return null;
    }
}
