package traci.model.csg;

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
    
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        /**
         * The sphere has the equation:
         * x^2 + y^2 + z^2 = 1
         */
        
        final double c = dir.x * dir.x + dir.y * dir.y + dir.z * dir.z;
        final double a = 2 * (p.x * dir.x + p.y * dir.y + p.z * dir.z) / c;
        final double b = (p.x * p.x + p.y * p.y + p.z * p.z - 1) / c;
        
        final double d = (a*a)/4 - b;
        
        if (d > 0)
        {
            final double sqrtD = Math.sqrt(d);
            
            final double t0 = -a/2 - sqrtD;
            if (t0 <= EPSILON) return null;
            final double t1 = -a/2 + sqrtD;
            
            final Vector normal0 = p.add(dir.mul(t0));
            final Vector normal1 = p.add(dir.mul(t1));
            
            final Point p0 = new Point(t0, this, normal0);
            final Point p1 = new Point(t1, this, normal1);
            
            return new Ray(new Interval(p0, p1));
        }
        
        return null;
    }
}
