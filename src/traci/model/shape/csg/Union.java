package traci.model.shape.csg;

import traci.math.Vector;
import traci.render.Ray2;

public class Union extends Csg
{
    @Override
    public Ray2 shootRay2(final Vector p, final Vector dir)
    {
        if (bBox != null && !bBox.test(p, dir))
        {
            return null;
        }

        Ray2 ray = null;

        for (int i = 0; i < numShapes; ++i)
        {
            ray = Ray2.union(ray, shapes.get(i).shootRay2(p, dir));
        }

        return ray;
    }
}
