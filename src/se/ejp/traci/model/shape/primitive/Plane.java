package se.ejp.traci.model.shape.primitive;

import se.ejp.traci.math.Vector;
import se.ejp.traci.render.Ray;
import se.ejp.traci.render.Point.Type;

public class Plane extends Primitive
{
    @Override
    public Vector primitiveGetNormalAt(final Vector p)
    {
        return Vector.UNIT_Y;
    }

    /**
     * The plane is defined by the equation y = 0.
     */
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        final double t = -p.y() / dir.y();

        if (t > -EPSILON)
        {
            final Ray ray = Ray.make();
            ray.add(t, this, Type.INTERSECT);
            return ray;
        }

        return null;
    }

    @Override
    public String toString()
    {
        return "Plane";
    }
}
