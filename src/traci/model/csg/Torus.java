package traci.model.csg;

import traci.math.Vector;
import traci.model.texture.Texture;
import traci.render.Ray;

public class Torus extends Primitive
{
    final double r;
    
    public Torus(final double r)
    {
        this(null, r);
    }
    
    public Torus(final Texture texture, final double r)
    {
        super(texture);
        
        this.r = r;
    }
    
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        throw new UnsupportedOperationException();
    }
}
