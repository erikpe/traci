package traci.model.shape.csg;

import traci.math.Vector;
import traci.model.material.Material;
import traci.model.shape.primitive.Primitive;
import traci.render.IntersectionStack;
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
        
        if (!shapes.get(0).isInside(p, primitive))
        {
            return false;
        }
        
        for (int i = 1; i < numShapes; ++i)
        {
            if (!shapes.get(i).isOutside(p, primitive))
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
        
        if (!shapes.get(0).isOutside(p, primitive))
        {
            return true;
        }
        
        for (int i = 1; i < numShapes; ++i)
        {
            if (shapes.get(i).isInside(p, primitive))
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
                    if (isecObj == 0 && testObj == 0)
                    {
                        continue;
                    }
                    
                    final Vector hitP = p.add(dir.mul(localStack.dists[isecNr]));
                    final Primitive primitive = localStack.objs[isecNr];
                    
                    if (isecObj == 0)
                    {
                        if (shapes.get(testObj).isInside(hitP, primitive))
                        {
                            keepIsec = false;
                            break;
                        }
                    }
                    else if (testObj == 0)
                    {
                        if (!shapes.get(testObj).isInside(hitP, primitive))
                        {
                            keepIsec = false;
                            break;
                        }
                    }
                    else if (!shapes.get(testObj).isOutside(hitP, primitive))
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
