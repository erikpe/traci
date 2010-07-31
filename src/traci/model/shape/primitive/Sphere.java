package traci.model.shape.primitive;

import traci.math.Vector;
import traci.model.material.Material;
import traci.render.IntersectionStack;
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
        final double c = dir.dot(dir);
        final double a = 2 * p.dot(dir) / c;
        final double b = (p.dot(p) - 1) / c;
        
        final double d = (a * a) / 4 - b;
        
        if (d > 0)
        {
            final double sqrtD = Math.sqrt(d);
            
            final double t0 = -a / 2 - sqrtD;
            
            if (t0 <= EPSILON)
            {
                return null;
            }
            
            final double t1 = -a / 2 + sqrtD;
            
            final Point p0 = Point.make(t0, this);
            final Point p1 = Point.make(t1, this);
            
            return new Ray(Interval.make(p0, p1));
        }
        
        return null;
    }
    
    @Override
    protected boolean primitiveIsInside(final Vector p)
    {
        return p.length() < 1.0 + INSIDE_MARIGIN;
    }
    
    @Override
    protected boolean primitiveIsOutside(final Vector p)
    {
        return p.length() > 1.0 - INSIDE_MARIGIN;
    }
    
    @Override
    protected void primitiveAllIntersections(final IntersectionStack iStack,
            final Vector p, final Vector dir)
    {
        final Ray ray = primitiveShootRay(p, dir);
        
        if (ray == null)
        {
            return;
        }
        
        for (final Interval ival : ray)
        {
            iStack.push(ival.p0().dist(), ival.p0().obj());
            
            if (ival.p1().dist() > ival.p0().dist())
            {
                iStack.push(ival.p1().dist(), ival.p1().obj());
            }
        }
    }
}
