package traci.model.shape.csg;

import traci.math.Vector;
import traci.model.material.Material;
import traci.render.Ray;

public class Union extends Csg
{
    public Union()
    {
        this(null);
    }
    
    public Union(final Material material)
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
        
        Ray ray = null;
        
        for (int i = 0; i < numShapes; ++i)
        {
            final Ray shapeRay = shapes.get(i).shootRay(p, dir);
            
            if (ray == null)
            {
                ray = shapeRay;
            }
            else if (shapeRay != null)
            {
                ray.merge(shapeRay);
            }
        }
        
        return ray;
    }
    
    @Override
    public boolean isInside(final Vector p)
    {
        final int numShapes = shapes.size();
        
        for (int i = 0; i < numShapes; ++i)
        {
            if (shapes.get(i).isInside(p))
            {
                return true;
            }
        }
        
        return false;
    }
}
