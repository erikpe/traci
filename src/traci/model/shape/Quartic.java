package traci.model.shape;

import java.util.Arrays;

import traci.math.Vector;
import traci.model.material.Material;
import traci.render.Ray;

public class Quartic extends Primitive
{
    public static final int NUM_COEFFS = 35;
    
    private final double[] coeffs;
    
    public Quartic(final double ... coeffs)
    {
        this(null, coeffs);
    }
    
    public Quartic(final Material material, final double ... coeffs)
    {
        super(material);
        
        assert coeffs.length == NUM_COEFFS;
        this.coeffs = Arrays.copyOf(coeffs, NUM_COEFFS);
    }
    
    /**
     * a {@link Quartic} is a polynomial shape of the fourth degree.
     */
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        throw new UnsupportedOperationException();
    }
}
