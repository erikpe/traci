package traci.model.shape.primitive;

import traci.math.Vector;
import traci.render.Point2.Type;
import traci.render.Ray2;

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
    public Ray2 primitiveShootRay2(final Vector p, final Vector dir)
    {
        final double t = -p.y() / dir.y();
        
        if (t > -EPSILON)
        {
            final Ray2 ray = Ray2.make();
            ray.add(t, this, Type.INTERSECT);
            return ray;
        }
        
        return null;
    }
}
