package traci.model.csg;

import java.util.List;

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
        final List<Shape> shapes = getShapes();
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
            ray.subtract(shapes.get(i).shootRay(p, dir));
            
            if (ray.isEmpty())
            {
                return null;
            }
        }
        
        return ray;
    }
}
