package traci.model.shape.csg;

import traci.math.Vector;
import traci.model.material.Material;
import traci.model.shape.primitive.Primitive;
import traci.render.IntersectionStack;
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
    
    @Deprecated
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
    public boolean isInside(final Vector p, final Primitive primitive)
    {
        if (bBox != null && !bBox.isInside(p))
        {
            return false;
        }
        
        final int numShapes = shapes.size();
        
        if (numShapes == 0)
        {
            return false;
        }
        
        for (int i = 0; i < numShapes; ++i)
        {
            if (!shapes.get(i).isInside(p, primitive))
            {
                return false;
            }
        }
        
        return true;
    }
    
    @Override
    public boolean isOutside(final Vector p, final Primitive primitive)
    {
        if (bBox != null && bBox.isOutside(p))
        {
            return true;
        }
        
        final int numShapes = shapes.size();
        
        if (numShapes == 0)
        {
            return false;
        }
        
        for (int i = 0; i < numShapes; ++i)
        {
            if (shapes.get(i).isOutside(p, primitive))
            {
                return true;
            }
        }
        
        return false;
    }
    
    @Override
    public void allIntersections(final IntersectionStack iStack,
            final Vector p, final Vector dir)
    {
        if (bBox != null && !bBox.test(p, dir))
        {
            return;
        }
        
        final int numShapes = shapes.size();
        final IntersectionStack localStack = IntersectionStack.make();
        
        for (int isecObj = 0; isecObj < numShapes; ++isecObj)
        {
            shapes.get(isecObj).allIntersections(localStack, p, dir);
            
            for (int isecNr = 0; isecNr < localStack.size(); ++isecNr)
            {
                boolean keepIsec = true;
                
                for (int testObj = 0; testObj < numShapes; ++testObj)
                {
                    if (isecObj == testObj)
                    {
                        continue;
                    }
                    
                    if (!shapes.get(testObj).isInside(p.add(dir.mul(localStack.dists[isecNr])),
                            localStack.objs[isecNr]))
                    {
                        keepIsec = false;
                        break;
                    }
                }
                
                if (keepIsec)
                {
                    iStack.push(localStack.dists[isecNr], localStack.objs[isecNr]);
                }
            }
            
            localStack.reset();
        }
    }
}
