package traci.model.shape.primitive;

import traci.math.Vector;
import traci.model.material.Material;
import traci.render.IntersectionStack;
import traci.render.Interval;
import traci.render.Point;
import traci.render.Point2;
import traci.render.Ray;
import traci.render.Ray2;
import traci.render.Point2.Type;

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
    
    public Ray2 primitiveShootRay2(final Vector p, final Vector dir)
    {
        final double c = dir.dot(dir);
        final double a = 2 * p.dot(dir) / c;
        final double b = (p.dot(p) - 1) / c;
        
        final double d = (a * a) / 4 - b;
        
        if (d > 0)
        {
            final double sqrtD = Math.sqrt(d);
            final double ma2 = -a / 2;
            
            final double t0 = ma2 - sqrtD;
            
            if (t0 <= EPSILON)
            {
                return null;
            }
            
            final double t1 = ma2 + sqrtD;
            
            final Ray2 ray = Ray2.make();
            
            ray.add(Point2.make(t0, this, Type.ENTER));
            ray.add(Point2.make(t1, this, Type.LEAVE));
            
            return ray;
        }
        
        return null;
    }
    
    /**
     * The sphere is define by the equation
     * 
     * x^2 + y^2 + z^2 = 1
     */
    @Deprecated
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
        final double c = dir.dot(dir);
        final double a = 2 * p.dot(dir) / c;
        final double b = (p.dot(p) - 1) / c;
        
        final double d = (a * a) / 4 - b;
        
        if (d > 0)
        {
            final double sqrtD = Math.sqrt(d);
            final double ma2 = -a / 2;
            
            final double t0 = ma2 - sqrtD;
            
            if (t0 <= EPSILON)
            {
                return;
            }
            
            final double t1 = ma2 + sqrtD;
            
            iStack.push(t0, this);
            iStack.push(t1, this);
        }
    }
}
