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
        final double a = dir.dot(dir);
        final double b = 2 * (p.dot(dir));
        final double g = (p.dot(p)) - r * r - 1;
        
        final double a4 = a * a;
        final double a3 = 2 * a * b;
        final double a2 = (b * b) + (2 * a * g) + (4 * dir.z() * dir.z());
        final double a1 = (2 * b * g) + (8 * p.z() * dir.z());
        final double a0 = (g * g) + (4 * p.z() * p.z()) - (4 * r * r);
        
        return null;
    }
}
