package traci.model.csg;

import java.util.List;

import traci.math.Vector;
import traci.model.texture.Texture;
import traci.render.Ray;

public class Union extends Csg
{
    public Union()
    {
        this(null);
    }
    
    public Union(final Texture material)
    {
        super(material);
    }
    
    @Override
    public Ray shootRay(final Vector p, final Vector dir)
    {
        final List<Shape> shapes = getShapes();
        final int numShapes = shapes.size();
        
        Ray ray = null;
        
        for (int i = 0; i < numShapes; ++i)
        {
            final Ray shapeRay = shapes.get(i).shootRay(p, dir);
            
            if (ray == null)
            {
                ray = shapeRay;
            }
            else
            {
                ray.add(shapeRay);
            }
        }
        
        return ray;
    }
}
