package traci.model.csg;

import traci.math.Vector;
import traci.model.texture.Texture;
import traci.render.Ray;
import traci.render.Interval;
import traci.render.Point;

public class Cylinder extends Primitive
{
    public Cylinder()
    {
        this(null);
    }
    
    public Cylinder(final Texture material)
    {
        super(material);
    }
    
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        Ray ray = null;
        
        /**
         * Begin with plane y = 0 and y = 1
         */
        final Point y0 = new Point((0.0 - p.y) / dir.y, this, Vector.UNIT_NEG_Y);
        final Point y1 = new Point((1.0 - p.y) / dir.y, this, Vector.UNIT_Y);
        
        Point near = Point.nearest(y0, y1);
        Point far = Point.farest(y0, y1);
        
        /**
         * Then the infinite cylinder x^2 + z^2 = 1
         */
        final double a = (2*p.x*dir.x + 2*p.z*dir.z) / (dir.x*dir.x + dir.z*dir.z);
        final double b = (p.x*p.x + p.z*p.z - 1) / (dir.x*dir.x + dir.z*dir.z);
        
        if ((a*a)/4 - b > 0)
        {
            final double t0 = -a/2 - Math.sqrt((a*a)/4 - b);
            final double t1 = -a/2 + Math.sqrt((a*a)/4 - b);
            
            final Point p0 = new Point(t0, this, Vector.make(p.x, 0, p.z).add(Vector.make(dir.x, 0, dir.z).mul(t0)));
            final Point p1 = new Point(t1, this, Vector.make(p.x, 0, p.z).add(Vector.make(dir.x, 0, dir.z).mul(t1)));
            
            near = Point.farest(near, p0);
            far = Point.nearest(far, p1);
            
            if (near.dist > 0 && near.dist < far.dist)
            {
                ray = new Ray(new Interval(near, far));
            }
        }
        
        return ray;
    }
}
