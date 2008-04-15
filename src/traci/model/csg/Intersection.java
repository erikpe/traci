package traci.model.csg;

import java.util.List;

import traci.math.Vector;
import traci.model.texture.Texture;
import traci.render.Intervals;

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
    public Intervals shootRay(final Vector p, final Vector lookAt)
    {
        final List<Shape> shapes = getShapes();
        final int numShapes = shapes.size();
        
        if (numShapes <= 0)
        {
            return null;
        }
        
        Intervals ivals = shapes.get(0).shootRay(p, lookAt);
        
        if (ivals == null)
        {
            return null;
        }
        
        for (int i = 1; i < numShapes; ++i)
        {
            final Intervals shapeIvals = shapes.get(i).shootRay(p, lookAt);
            
            if (shapeIvals == null)
            {
                return null;
            }
            
            ivals.intersect(shapeIvals);
            
            if (ivals.isEmpty())
            {
                return null;
            }
        }
        
        return ivals;
    }
}
