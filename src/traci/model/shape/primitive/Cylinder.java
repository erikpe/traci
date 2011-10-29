package traci.model.shape.primitive;

import traci.math.Transformations;
import traci.math.Vector;
import traci.render.Point2.Type;
import traci.render.Ray2;

public class Cylinder extends Primitive
{
    public Cylinder(final double radius, final Vector v0, final Vector v1)
    {
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
    public Ray2 primitiveShootRay2(final Vector p, final Vector dir)
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
                final Ray2 ray = Ray2.make();

                ray.add(near, this, Type.ENTER);
                ray.add(far, this, Type.LEAVE);

                return ray;
            }
        }

        return null;
    }
}
