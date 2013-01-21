package traci.model.shape.primitive;

import traci.math.Vector;
import traci.render.Point.Type;
import traci.render.Ray;

public class Box extends Primitive
{
    @Override
    public Vector primitiveGetNormalAt(final Vector p)
    {
        final double absX = Math.abs(p.x() - 0.5);
        final double absY = Math.abs(p.y() - 0.5);
        final double absZ = Math.abs(p.z() - 0.5);

        if (absX > absY)
        {
            if (absX > absZ)
            {
                return Vector.UNIT_X;
            }

            return Vector.UNIT_Z;
        }
        else if (absY > absZ)
        {
            return Vector.UNIT_Y;
        }

        return Vector.UNIT_Z;
    }

    /**
     * The box is bounded by the planes x = 0, x = 1, y = 0, y = 1, z = 0 and z = 1.
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

        final double x0 = -px / dirx;
        final double x1 = (1.0 - px) / dirx;

        double near = min(x0, x1);
        double far = max(x0, x1);

        final double y0 = -py / diry;
        final double y1 = (1.0 - py) / diry;

        near = max(near, min(y0, y1));
        far = min(far, max(y0, y1));

        final double z0 = -pz / dirz;
        final double z1 = (1.0 - pz) / dirz;

        near = max(near, min(z0, z1));
        far = min(far, max(z0, z1));

        if (far > -EPSILON && near < far)
        {
            final Ray ray = Ray.make();

            ray.add(near, this, Type.ENTER);
            ray.add(far, this, Type.LEAVE);

            return ray;
        }

        return null;
    }

    @Override
    public String toString()
    {
        return "box";
    }
}
