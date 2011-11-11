package traci.model.shape.csg;

import traci.math.Vector;
import traci.render.Ray;

public class Intersection extends Csg
{
    @Override
    public Ray shootRay(final Vector p, final Vector dir)
    {
        if ((bBox != null && !bBox.test(p, dir)) || numShapes == 0)
        {
            return null;
        }

        Ray ray = shapes[0].shootRay(p, dir);

        if (ray == null)
        {
            return null;
        }

        for (int i = 1; i < numShapes; ++i)
        {
            ray = Ray.intersect(ray, shapes[i].shootRay(p, dir));

            if (ray == null)
            {
                return null;
            }
        }

        return ray;
    }
}
