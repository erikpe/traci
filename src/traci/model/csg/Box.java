package traci.model.csg;

import traci.math.Vector;
import traci.model.texture.Texture;
import traci.render.Intervals;
import traci.render.Ival;
import traci.render.Point;

public class Box extends Primitive
{
    public Box()
    {
        this(null);
    }
    
    public Box(final Texture material)
    {
        super(material);
    }
    
    @Override
    public Intervals primitiveShootRay(final Vector p, final Vector dir)
    {
        Intervals ivals = null;
        
        /**
         * Plane x = 0 and x = 1
         */
        final Point x0 = new Point((0.0 - p.x) / dir.x, this, Vector.UNIT_NEG_X);
        final Point x1 = new Point((1.0 - p.x) / dir.x, this, Vector.UNIT_X);
        
        Point near = Point.nearest(x0, x1);
        Point far = Point.farest(x0, x1);
        
        /**
         * Plane y = 0 and y = 1
         */
        final Point y0 = new Point((0.0 - p.y) / dir.y, this, Vector.UNIT_NEG_Y);
        final Point y1 = new Point((1.0 - p.y) / dir.y, this, Vector.UNIT_Y);
        
        near = Point.farest(near, Point.nearest(y0, y1));
        far = Point.nearest(far, Point.farest(y0, y1));
        
        /**
         * Plane z = 0 and z = 1
         */
        final Point z0 = new Point((0.0 - p.z) / dir.z, this, Vector.UNIT_NEG_Z);
        final Point z1 = new Point((1.0 - p.z) / dir.z, this, Vector.UNIT_Z);
        
        near = Point.farest(near, Point.nearest(z0, z1));
        far = Point.nearest(far, Point.farest(z0, z1));
        
        if (near.dist < far.dist)
        {
            ivals = new Intervals(new Ival(near, far));
        }
        
        return ivals;
    }
}
