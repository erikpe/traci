package traci.model.shape;

import java.util.concurrent.atomic.AtomicLong;

import traci.math.Transformable;
import traci.math.Transformation;
import traci.math.Transformations;
import traci.math.Vector;

public class BoundingBox implements Transformable, Cloneable
{
    public static final AtomicLong miss = new AtomicLong(1);
    public static final AtomicLong hit = new AtomicLong(1);
    
    private Transformation transformation;
    
    public BoundingBox()
    {
        transformation = Transformations.identity();
    }
    
    @Override
    public void transform(final Transformation tr)
    {
        transformation = transformation.compose(tr);
    }
    
    private final double min(final double val0, final double val1)
    {
        return val0 < val1 ? val0 : val1;
    }
    
    private final double max(final double val0, final double val1)
    {
        return val0 > val1 ? val0 : val1;
    }
    
    public boolean test(final Vector p, final Vector dir)
    {
        final Vector transP = transformation.pointInv(p);
        final Vector transDir = transformation.dirInv(dir);
        
        /**
         * Plane x = 0 and x = 1
         */
        final double x0 = -transP.x() / transDir.x();
        final double x1 = (1.0 - transP.x()) / transDir.x();
        
        double far = max(x0, x1);
        double near = min(x0, x1);
        
        if (far < 0)
        {
            //miss.incrementAndGet();
            return false;
        }
        
        /**
         * Plane y = 0 and y = 1
         */
        final double y0 = -transP.y() / transDir.y();
        final double y1 = (1.0 - transP.y()) / transDir.y();
        
        far = min(far, max(y0, y1));
        near = max(near, min(y0, y1));
        
        if (far < 0 || far < near)
        {
            //miss.incrementAndGet();
            return false;
        }
        
        /**
         * Plane z = 0 and z = 1
         */
        final double z0 = -transP.z() / transDir.z();
        final double z1 = (1.0 - transP.z()) / transDir.z();
        
        near = max(near, min(z0, z1));
        far = min(far, max(z0, z1));
        
        if (far > 0 && near < far)
        {
            //hit.incrementAndGet();
            return true;
        }
        
        //miss.incrementAndGet();
        return false;
    }
    
    @Override
    public Object clone()
    {
        try
        {
            final BoundingBox res = (BoundingBox) super.clone();
            res.transformation = transformation;
            
            return res;
        }
        catch (final CloneNotSupportedException e)
        {
            e.printStackTrace();
            return null;
        }
    }
}
