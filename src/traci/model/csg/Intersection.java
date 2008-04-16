package traci.model.csg;

import java.util.List;

import traci.math.Vector;
import traci.model.texture.Texture;
import traci.render.Ray;

public class Intersection extends Csg
{
    public Intersection()
    {
        this(null);
    }
    
    public Intersection(final Texture material)
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
}
