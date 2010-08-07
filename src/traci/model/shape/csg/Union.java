package traci.model.shape.csg;

import traci.math.Vector;
import traci.model.material.Material;
import traci.model.shape.primitive.Primitive;
import traci.render.IntersectionStack;
import traci.render.Ray;
import traci.render.Ray2;

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
    
    public Ray2 shootRay2(final Vector p, final Vector dir)
    {
        if (bBox != null && !bBox.test(p, dir))
        {
            return null;
        }
        
        Ray2 ray = null;
        
        for (int i = 0; i < numShapes; ++i)
        {
            int len = 23;
            double d0 = 17;
            double d1 = 17;
            
            if (ray != null)
            {
                len = ray.size;
                
                if (len >= 1)
                {
                    d0 = ray.points[0].dist();
                }
                if (len >= 2)
                {
                    d1 = ray.points[1].dist();
                }
            }
            
            assert Ray2.checkRay(ray);
            ray = Ray2.union(ray, shapes.get(i).shootRay2(p, dir));
            assert Ray2.checkRay(ray);
        }
        
        return ray;
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
