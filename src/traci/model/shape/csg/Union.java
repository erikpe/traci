package traci.model.shape.csg;

import traci.math.Vector;
import traci.model.material.Material;
import traci.model.shape.primitive.Primitive;
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
    
    @Deprecated
    @Override
    public Ray shootRay(final Vector p, final Vector dir)
    {
        if (bBox != null && !bBox.test(p, dir))
        {
            return null;
        }
        
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
    public boolean isInside(final Vector p, final Primitive primitive)
    {
        if (bBox != null && !bBox.isInside(p))
        {
            return false;
        }
        
        for (int i = 0; i < numShapes; ++i)
        {
            if (shapes.get(i).isInside(p, primitive))
            {
                return true;
            }
        }
        
        return false;
    }
    
    @Override
    public boolean isOutside(final Vector p, final Primitive primitive)
    {
        if (bBox != null && bBox.isOutside(p))
        {
            return true;
        }
        
        for (int i = 0; i < numShapes; ++i)
        {
            if (!shapes.get(i).isOutside(p, primitive))
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
        
        for (int i = 0; i < numShapes; ++i)
        {
            shapes.get(i).allIntersections(iStack, p, dir);
        }
    }
    
    @Override
    public boolean anyIntersection(final Vector p, final Vector dir)
    {
        if (bBox != null && !bBox.test(p, dir))
        {
            return false;
        }
        
        for (int i = 0; i < numShapes; ++i)
        {
            if (shapes.get(i).anyIntersection(p, dir))
            {
                return true;
            }
        }
        
        return false;
    }
}
