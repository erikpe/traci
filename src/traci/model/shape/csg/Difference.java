package traci.model.shape.csg;

import traci.math.Vector;
import traci.render.Ray;

public class Difference extends Csg
{
    @Override
    public Ray shootRay(final Vector p, final Vector dir)
    {
        if ((bBox != null && !bBox.test(p, dir)) || numShapes == 0)
        {
            return null;
        }

        Ray ray = shapes.get(0).shootRay(p, dir);

        if (ray == null)
        {
            return null;
        }

        for (int i = 1; i < numShapes; ++i)
        {
            ray = Ray.difference(ray, shapes.get(i).shootRay(p, dir));

            if (ray == null)
            {
                return null;
            }
        }

        return ray;
    }
}
