package traci.model.shape.primitive;

import traci.math.Vector;
import traci.model.material.Material;
import traci.render.Interval;
import traci.render.Point;
import traci.render.Ray;

public class Box extends Primitive
{
    public Box()
    {
        this(null);
    }
    
    public Box(final Material material)
    {
        super(material);
    }
    
    @Override
    public Vector primitiveGetNormalAt(final Vector p)
    {
        final double absX = Math.abs(p.x() - 0.5);
        final double absY = Math.abs(p.y() - 0.5);
        final double absZ = Math.abs(p.z() - 0.5);
        
        if (absX > absY)
        {
            if (absX > absZ)
            {
                return Vector.UNIT_X;
            }
            
            return Vector.UNIT_Z;
        }
        else if (absY > absZ)
        {
            return Vector.UNIT_Y;
        }
        
        return Vector.UNIT_Z;
    }
    
    /**
     * The box is bounded by the planes x = 0, x = 1, y = 0, y = 1, z = 0 and z = 1.
     */
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        Ray ray = null;
        
        /**
         * Plane x = 0 and x = 1
         */
        final Point x0 = Point.make((0.0 - p.x()) / dir.x(), this);
        final Point x1 = Point.make((1.0 - p.x()) / dir.x(), this);
        
        Point near = Point.nearest(x0, x1);
        Point far = Point.farest(x0, x1);
        
        /**
         * Plane y = 0 and y = 1
         */
        final Point y0 = Point.make((0.0 - p.y()) / dir.y(), this);
        final Point y1 = Point.make((1.0 - p.y()) / dir.y(), this);
        
        near = Point.farest(near, Point.nearest(y0, y1));
        far = Point.nearest(far, Point.farest(y0, y1));
        
        /**
         * Plane z = 0 and z = 1
         */
        final Point z0 = Point.make((0.0 - p.z()) / dir.z(), this);
        final Point z1 = Point.make((1.0 - p.z()) / dir.z(), this);
        
        near = Point.farest(near, Point.nearest(z0, z1));
        far = Point.nearest(far, Point.farest(z0, z1));
        
        if (near.dist() > EPSILON && near.dist() < far.dist())
        {
            ray = new Ray(Interval.make(near, far));
        }
        
        return ray;
    }
    
    @Override
    protected boolean primitiveIsInside(final Vector p)
    {
        final double x = p.x();
        final double y = p.y();
        final double z = p.z();
        
        return x >= 0.0 && x <= 1.0
            && y >= 0.0 && y <= 1.0
            && z >= 0.0 && z <= 1.0;
    }
}
