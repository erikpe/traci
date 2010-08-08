package traci.model.shape.primitive;

import traci.math.Vector;
import traci.model.material.Material;
import traci.render.IntersectionStack;
import traci.render.Interval;
import traci.render.Point;
import traci.render.Point2.Type;
import traci.render.Ray;
import traci.render.Ray2;

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
        return Vector.UNIT_Y;
    }
    
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
    
    /**
     * The plane is defined by the equation y = 0.
     */
    @Deprecated
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
    
    @Override
    protected boolean primitiveIsInside(final Vector p)
    {
        return p.y() < INSIDE_MARIGIN && p.y() > -INSIDE_MARIGIN;
    }
    
    @Override
    protected boolean primitiveIsOutside(final Vector p)
    {
        return true;
    }
    
    @Override
    protected void primitiveAllIntersections(final IntersectionStack iStack,
            final Vector p, final Vector dir)
    {
        final double t = -p.y() / dir.y();
        
        if (t > EPSILON)
        {
            iStack.push(t, this);
        }
    }
}
