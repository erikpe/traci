package se.ejp.traci.model.shape.primitive;

import se.ejp.traci.math.Vector;
import se.ejp.traci.render.Point.Type;
import se.ejp.traci.render.Ray;

public class Plane extends Primitive
{
    private Plane() { }

    public static Plane make()
    {
        return new Plane();
    }

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
        final double dist = -p.y() / dir.y();

        if (dist > -EPSILON)
        {
            final Ray ray = Ray.make();
            ray.add(dist, this, Type.INTERSECT);
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
