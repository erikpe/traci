package se.ejp.traci.model.shape.csg;

import se.ejp.traci.math.Vector;
import se.ejp.traci.render.Ray;

public class Union extends Csg
{
    @Override
    public Ray shootRay(final Vector p, final Vector dir)
    {
        if (bBox != null && !bBox.test(p, dir))
        {
            return null;
        }

        Ray ray = null;

        for (int i = 0; i < numShapes; ++i)
        {
            ray = Ray.union(ray, shapes[i].shootRay(p, dir));
        }

        return ray;
    }

    @Override
    public String toString()
    {
        return "Union";
    }
}
