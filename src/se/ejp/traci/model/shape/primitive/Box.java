package se.ejp.traci.model.shape.primitive;

import se.ejp.traci.math.Transformations;
import se.ejp.traci.math.Vector;
import se.ejp.traci.render.Point.Type;
import se.ejp.traci.render.Ray;

public class Box extends Primitive
{
    private Box() { }

    public static Box make()
    {
        return new Box();
    }

    public static Box make(final Vector v0, final Vector v1)
    {
        final double xSize = Math.abs(v1.x() - v0.x());
        final double ySize = Math.abs(v1.y() - v0.y());
        final double zSize = Math.abs(v1.z() - v0.z());

        final double x = min(v0.x(), v1.x());
        final double y = min(v0.y(), v1.y());
        final double z = min(v0.z(), v1.z());

        final Box box = make();

        box.transform(Transformations.scale(xSize, ySize, zSize));
        box.transform(Transformations.translate(x, y, z));

        return box;
    }

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

            ray.add(near, this, Type.ENTER, null);
            ray.add(far, this, Type.LEAVE, null);

            return ray;
        }

        return null;
    }

    @Override
    public String toString()
    {
        return "Box";
    }
}
