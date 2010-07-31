package traci.model.shape.csg;

import traci.math.Vector;
import traci.model.material.Material;
import traci.render.IntersectionStack;
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
        if (bBox != null && !bBox.isInside(p))
        {
            return false;
        }
        
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
    
    @Override
    public boolean isOutside(final Vector p)
    {
        if (bBox != null && bBox.isOutside(p))
        {
            return true;
        }
        
        final int numShapes = shapes.size();
        
        for (int i = 0; i < numShapes; ++i)
        {
            if (!shapes.get(i).isOutside(p))
            {
                return false;
            }
        }
        
        return true;
    }
    
    @Override
    public void allIntersections(final IntersectionStack iStack, final Vector p,
            final Vector dir)
    {
        if (bBox != null && !bBox.test(p, dir))
        {
            return;
        }
        
        final int numShapes = shapes.size();
        
        for (int i = 0; i < numShapes; ++i)
        {
            shapes.get(i).allIntersections(iStack, p, dir);
        }
    }
}
