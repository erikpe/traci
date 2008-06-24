package traci.model.shape.primitive;

import traci.math.Vector;
import traci.model.material.Material;
import traci.render.Ray;

public class Torus extends Primitive
{
    final double r;
    
    public Torus(final double r)
    {
        this(null, r);
    }
    
    public Torus(final Material material, final double r)
    {
        super(material);
        
        this.r = r;
    }
    
    /**
     * The torus lies in the xz-plane, has a major radius of {@code 1}, and a
     * minor radius of {@code r}.
     * 
     * It is a special case of a {@link Quartic} surface.
     */
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        throw new UnsupportedOperationException();
    }
}
