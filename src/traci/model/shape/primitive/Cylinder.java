package traci.model.shape.primitive;

import traci.math.Transformations;
import traci.math.Vector;
import traci.model.material.Material;
import traci.render.IntersectionStack;
import traci.render.Interval;
import traci.render.Point;
import traci.render.Point2;
import traci.render.Point2.Type;
import traci.render.Ray;
import traci.render.Ray2;

public class Cylinder extends Primitive
{
    public Cylinder()
    {
        this(null);
    }
    
    public Cylinder(final Material material)
    {
        super(material);
    }
    
    public Cylinder(final double radius, final Vector v0, final Vector v1)
    {
        this();
        
        final double length = v1.sub(v0).length();
        
        scale(radius, length, radius);
        transform(Transformations.rotVecToVec(Vector.UNIT_Y, v1.sub(v0)));
        translate(v0);
    }
    
    @Override
    public Vector primitiveGetNormalAt(final Vector p)
    {
        final double x = p.x();
        final double y = p.y();
        final double z = p.z();
        
        final double dist = Math.sqrt(x * x + z * z);
        final double h = 2.0 * Math.abs(y - 0.5);
        
        if (h > dist)
        {
            return Vector.UNIT_Y;
        }
        
        return Vector.make(x, 0, z);
    }
    
    public Ray2 primitiveShootRay2(final Vector p, final Vector dir)
    {
        final double px = p.x();
        final double py = p.y();
        final double pz = p.z();
        
        final double dirx = dir.x();
        final double diry = dir.y();
        final double dirz = dir.z();
        
        final double y0 = -py / diry;
        final double y1 = (1.0 - py) / diry;
        
        double near = min(y0, y1);
        double far = max(y0, y1);
        
        final double g = dirx * dirx + dirz * dirz;
        final double a = (2 * px * dirx + 2 * pz * dirz) / g;
        final double b = (px * px + pz * pz - 1) / g;
        
        if ((a * a) / 4 - b > 0)
        {
            final double ma2 = -a / 2;
            final double sq = Math.sqrt((a * a) / 4 - b);
            
            final double t0 = ma2 - sq;
            final double t1 = ma2 + sq;
            
            near = max(near, t0);
            far = min(far, t1);
            
            if (near > EPSILON && near < far)
            {
                final Ray2 ray = Ray2.make();
                
//                ray.add(Point2.make(near, this, Type.ENTER));
//                ray.add(Point2.make(far, this, Type.LEAVE));
                ray.add(near, this, Type.ENTER);
                ray.add(far, this, Type.LEAVE);
                
                return ray;
            }
        }
        
        return null;
    }
    
    /**
     * The cylinder is bounded by the planes y = 0, y = 1 and the infinite
     * cylinder x^2 + z^2 = 1.
     */
    @Deprecated
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        Ray ray = null;
        
        /**
         * Begin with plane y = 0 and y = 1
         */
        final Point y0 = Point.make((0.0 - p.y()) / dir.y(), this);
        final Point y1 = Point.make((1.0 - p.y()) / dir.y(), this);
        
        Point near = Point.nearest(y0, y1);
        Point far = Point.farest(y0, y1);
        
        /**
         * Then the infinite cylinder x^2 + z^2 = 1
         */
        
        final double px = p.x();
        final double pz = p.z();
        final double dirx = dir.x();
        final double dirz = dir.z();
        
        final double a = (2 * px * dirx + 2 * pz * dirz)
                / (dirx * dirx + dirz * dirz);
        final double b = (px * px + pz * pz - 1)
                / (dirx * dirx + dirz * dirz);
        
        if ((a * a) / 4 - b > 0)
        {
            final double t0 = -a / 2 - Math.sqrt((a * a) / 4 - b);
            final double t1 = -a / 2 + Math.sqrt((a * a) / 4 - b);
            
            final Point p0 = Point.make(t0, this);
            final Point p1 = Point.make(t1, this);
            
            near = Point.farest(near, p0);
            far = Point.nearest(far, p1);
            
            if (near.dist() > EPSILON && near.dist() < far.dist())
            {
                ray = new Ray(Interval.make(near, far));
            }
        }
        
        return ray;
    }
    
    @Override
    protected boolean primitiveIsInside(final Vector p)
    {
        final double x = p.x();
        final double y = p.y();
        final double z = p.z();

        if (y > 1.0 + INSIDE_MARIGIN || y < -INSIDE_MARIGIN)
        {
            return false;
        }
        
        final double dist = Math.sqrt(x * x + z * z);
        
        return dist < 1.0 + INSIDE_MARIGIN;
    }
    
    @Override
    protected boolean primitiveIsOutside(final Vector p)
    {
        final double x = p.x();
        final double y = p.y();
        final double z = p.z();
        
        if (y > 1.0 - INSIDE_MARIGIN || y < INSIDE_MARIGIN)
        {
            return true;
        }
        
        final double dist = Math.sqrt(x * x + z * z);
        
        return dist > 1.0 - INSIDE_MARIGIN;
    }
    
    @Override
    protected void primitiveAllIntersections(final IntersectionStack iStack,
            final Vector p, final Vector dir)
    {
        final double px = p.x();
        final double py = p.y();
        final double pz = p.z();
        
        final double dirx = dir.x();
        final double diry = dir.y();
        final double dirz = dir.z();
        
        final double y0 = -py / diry;
        final double y1 = (1.0 - py) / diry;
        
        double near = min(y0, y1);
        double far = max(y0, y1);
        
        final double g = dirx * dirx + dirz * dirz;
        final double a = (2 * px * dirx + 2 * pz * dirz) / g;
        final double b = (px * px + pz * pz - 1) / g;
        
        if ((a * a) / 4 - b > 0)
        {
            final double ma2 = -a / 2;
            final double sq = Math.sqrt((a * a) / 4 - b);
            
            final double t0 = ma2 - sq;
            final double t1 = ma2 + sq;
            
            near = max(near, t0);
            far = min(far, t1);
            
            if (near > EPSILON && near < far)
            {
                iStack.push(near, this);
                iStack.push(far, this);
            }
        }
    }
}
