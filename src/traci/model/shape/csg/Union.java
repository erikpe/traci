package traci.model.shape.csg;

import traci.math.Vector;
import traci.render.Ray;

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
            ray = Ray.union(ray, shapes.get(i).shootRay(p, dir));
        }

        return ray;
    }
}
