package traci.model.csg;

import java.util.List;

import traci.math.Vector;
import traci.model.texture.Texture;
import traci.render.Intervals;

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
    public Intervals shootRay(final Vector p, final Vector lookAt)
    {
        final List<Shape> shapes = getShapes();
        final int numShapes = shapes.size();
        
        Intervals ivals = null;
        
        for (int i = 0; i < numShapes; ++i)
        {
            final Intervals shapeIvals = shapes.get(i).shootRay(p, lookAt);
            
            if (ivals == null)
            {
                ivals = shapeIvals;
            }
            else
            {
                ivals.add(shapeIvals);
            }
        }
        
        return ivals;
    }
}
