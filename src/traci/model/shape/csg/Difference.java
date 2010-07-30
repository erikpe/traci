package traci.model.shape.csg;

import traci.math.Vector;
import traci.model.material.Material;
import traci.render.Ray;

public class Difference extends Csg
{
    public Difference()
    {
        this(null);
    }
    
    public Difference(final Material material)
    {
        super(material);
    }
    
    @Override
    public Ray shootRay(final Vector p, final Vector dir)
    {
        if (bBox != null && !bBox.test(p, dir))
        {
            return null;
        }
        
        final int numShapes = shapes.size();
        
        if (numShapes <= 0)
        {
            return null;
        }
        
        Ray ray = shapes.get(0).shootRay(p, dir);
        
        if (ray == null)
        {
            return null;
        }
        
        for (int i = 1; i < numShapes; ++i)
        {
            final Ray shapeRay = shapes.get(i).shootRay(p, dir);
            
            if (shapeRay != null)
            {
                ray.subtract(shapeRay);
            }
            
            if (ray.isEmpty())
            {
                return null;
            }
        }
        
        return ray;
    }
    
    @Override
    public boolean isInside(final Vector p)
    {
        final int numShapes = shapes.size();
        
        if (numShapes == 0)
        {
            return false;
        }
        
        if (!shapes.get(0).isInside(p))
        {
            return false;
        }
        
        for (int i = 1; i < numShapes; ++i)
        {
            if (shapes.get(i).isInside(p))
            {
                return false;
            }
        }
        
        return true;
    }
}
