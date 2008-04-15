package traci.model.csg;

import traci.math.Vector;
import traci.model.texture.Texture;
import traci.render.Intervals;
import traci.render.Ival;
import traci.render.Point;

public class Sphere extends Primitive
{
    public Sphere()
    {
        this(null);
    }
    
    public Sphere(final Texture material)
    {
        super(material);
    }
    
    @Override
    public Intervals primitiveShootRay(final Vector p, final Vector dir)
    {
        Intervals ivals = null;
        
        double a = (p.mul(2).dot(dir)) / dir.dot(dir);
        double b = (p.dot(p)-1) / (dir.dot(dir));
        
        if ((a*a)/4 - b > 0)
        {
            final double t0 = -a/2 - Math.sqrt((a*a)/4 - b);
            final double t1 = -a/2 + Math.sqrt((a*a)/4 - b);
            
            final Vector normal0 = p.add(dir.mul(t0));
            final Vector normal1 = p.add(dir.mul(t1));
            
            final Point p0 = new Point(t0, this, normal0);
            final Point p1 = new Point(t1, this, normal1);
            
            ivals = new Intervals(new Ival(p0, p1));
        }
        
        return ivals;
    }
}
