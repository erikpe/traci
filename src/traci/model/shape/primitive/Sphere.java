package traci.model.shape.primitive;

import traci.math.Vector;
import traci.model.material.Material;
import traci.render.Interval;
import traci.render.Point;
import traci.render.Ray;

public class Sphere extends Primitive
{
    public Sphere()
    {
        this(null);
    }
    
    public Sphere(final Material material)
    {
        super(material);
    }
    
    public Vector primitiveGetNormalAt(final Vector p)
    {
        return p;
    }
    
    /**
     * The sphere is define by the equation
     * 
     * x^2 + y^2 + z^2 = 1
     */
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        final double c = dir.x() * dir.x() + dir.y() * dir.y() + dir.z() * dir.z();
        final double a = 2 * (p.x() * dir.x() + p.y() * dir.y() + p.z() * dir.z()) / c;
        final double b = (p.x() * p.x() + p.y() * p.y() + p.z() * p.z() - 1) / c;
        
        final double d = (a * a) / 4 - b;
        
        if (d > 0)
        {
            final double sqrtD = Math.sqrt(d);
            
            final double t0 = -a/2 - sqrtD;
            if (t0 <= EPSILON) return null;
            final double t1 = -a/2 + sqrtD;
            
            final Point p0 = Point.make(t0, this);
            final Point p1 = Point.make(t1, this);
            
            return new Ray(Interval.make(p0, p1));
        }
        
        return null;
    }
}
