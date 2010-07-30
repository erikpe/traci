package traci.model.shape.primitive;

import traci.math.Transformations;
import traci.math.Vector;
import traci.model.material.Material;
import traci.render.Interval;
import traci.render.Point;
import traci.render.Ray;

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
    
    /**
     * The cylinder is bounded by the planes y = 0, y = 1 and the infinite
     * cylinder x^2 + z^2 = 1.
     */
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
        final double a = (2 * p.x() * dir.x() + 2 * p.z() * dir.z())
                / (dir.x() * dir.x() + dir.z() * dir.z());
        final double b = (p.x() * p.x() + p.z() * p.z() - 1)
                / (dir.x() * dir.x() + dir.z() * dir.z());
        
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
}
