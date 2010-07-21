package traci.model.shape.csg;

import java.util.List;

import traci.math.Vector;
import traci.model.material.Material;
import traci.model.shape.Shape;
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
            else if (shapeRay != null)
            {
                ray.merge(shapeRay);
            }
        }
        
        return ray;
    }
}
