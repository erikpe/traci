package traci.model.shape.csg;

import traci.math.Vector;
import traci.render.Ray2;

public class Intersection extends Csg
{
    @Override
    public Ray2 shootRay2(final Vector p, final Vector dir)
    {
        if ((bBox != null && !bBox.test(p, dir)) || numShapes == 0)
        {
            return null;
        }

        Ray2 ray = shapes.get(0).shootRay2(p, dir);

        if (ray == null)
        {
            return null;
        }

        for (int i = 1; i < numShapes; ++i)
        {
            ray = Ray2.intersect(ray, shapes.get(i).shootRay2(p, dir));

            if (ray == null)
            {
                return null;
            }
        }

        return ray;
    }
}
