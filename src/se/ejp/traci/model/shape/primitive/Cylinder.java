package se.ejp.traci.model.shape.primitive;

import se.ejp.traci.math.Transformations;
import se.ejp.traci.math.Vector;
import se.ejp.traci.render.Ray;
import se.ejp.traci.render.Point.Type;

public class Cylinder extends Primitive
{
    public Cylinder()
    {
        super();
    }

    public Cylinder(final Double radius, final Vector v0, final Vector v1)
    {
        this();

        final double length = v1.sub(v0).length();

        transform(Transformations.scale(radius, length, radius));
        transform(Transformations.rotVecToVec(Vector.UNIT_Y, v1.sub(v0)));
        transform(Transformations.translate(v0));
    }

    @Override
    public Vector primitiveGetNormalAt(final Vector p)
    {
        final double x = p.x();
        final double y = p.y();
        final double z = p.z();

        final double dist = Math.sqrt(x * x + z * z);
        final double h = 2.0 * Math.abs(y - 0.5);

        if (h > dist)
        {
            return Vector.UNIT_Y;
        }

        return Vector.make(x, 0, z);
    }

    /**
     * The cylinder is bounded by the planes y = 0, y = 1 and the infinite
     * cylinder x^2 + z^2 = 1.
     */
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        final double px = p.x();
        final double py = p.y();
        final double pz = p.z();

        final double dirx = dir.x();
        final double diry = dir.y();
        final double dirz = dir.z();

        final double y0 = -py / diry;
        final double y1 = (1.0 - py) / diry;

        double near = min(y0, y1);
        double far = max(y0, y1);

        final double g = dirx * dirx + dirz * dirz;
        final double a = (2 * px * dirx + 2 * pz * dirz) / g;
        final double b = (px * px + pz * pz - 1) / g;

        if ((a * a) / 4 - b > 0)
        {
            final double ma2 = -a / 2;
            final double sq = Math.sqrt((a * a) / 4 - b);

            final double t0 = ma2 - sq;
            final double t1 = ma2 + sq;

            near = max(near, t0);
            far = min(far, t1);

            if (far > -EPSILON && near < far)
            {
                final Ray ray = Ray.make();

                ray.add(near, this, Type.ENTER);
                ray.add(far, this, Type.LEAVE);

                return ray;
            }
        }

        return null;
    }

    @Override
    public String toString()
    {
        return "Cylinder";
    }
}
