package traci.model.csg;

import traci.math.Vector;
import traci.model.material.Material;
import traci.render.Interval;
import traci.render.Point;
import traci.render.Ray;

public class Plane extends Primitive
{
    public Plane()
    {
        this(null);
    }
    
    public Plane(final Material material)
    {
        super(material);
    }
    
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        /**
         * Plane:
         * y = 0
         */
        final double t = -p.y / dir.y;
        
        if (t <= 0)
        {
            return null;
        }
        
        final Vector n0 = (p.y > 0 ? Vector.UNIT_Y : Vector.UNIT_NEG_Y);
        final Vector n1 = (p.y > 0 ? Vector.UNIT_NEG_Y : Vector.UNIT_Y);
        
        final Point p0 = new Point(t, this, n0);
        final Point p1 = new Point(t, this, n1);
        
        return new Ray(new Interval(p0, p1));
    }
    
}
