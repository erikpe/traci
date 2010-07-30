package traci.model.shape.csg;

import traci.math.Vector;
import traci.model.material.Material;
import traci.render.Ray;

public class Intersection extends Csg
{
    public Intersection()
    {
        this(null);
    }
    
    public Intersection(final Material material)
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
            
            if (shapeRay == null)
            {
                return null;
            }
            
            ray.intersect(shapeRay);
            
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
        
        for (int i = 0; i < numShapes; ++i)
        {
            if (!shapes.get(i).isInside(p))
            {
                return false;
            }
        }
        
        return true;
    }
}
