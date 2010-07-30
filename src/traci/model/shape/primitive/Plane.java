package traci.model.shape.primitive;

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
    public Vector primitiveGetNormalAt(final Vector p)
    {
        final Vector normal = Vector.UNIT_Y;
        return transformation.normal(normal);
    }
    
    /**
     * The plane is defined by the equation y = 0.
     */
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        final double t = -p.y() / dir.y();
        
        if (t <= EPSILON)
        {
            return null;
        }
        
        final Point p0 = Point.make(t, this);
        final Point p1 = Point.make(t, this);
        
        return new Ray(Interval.make(p0, p1));
    }
    
}
