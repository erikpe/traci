package traci.model.shape.primitive;

import java.util.Arrays;

import traci.math.Vector;
import traci.model.material.Material;
import traci.render.Ray;

public class Cubic extends Primitive
{
    public static final int NUM_COEFFS = 20;
    
    private final double[] coeffs;
    
    public Cubic(final double ... coeffs)
    {
        this(null, coeffs);
    }
    
    public Cubic(final Material material, final double ... coeffs)
    {
        super(material);
        
        assert coeffs.length == NUM_COEFFS;
        this.coeffs = Arrays.copyOf(coeffs, NUM_COEFFS);
    }
    
    /**
     * A {@link Cubic} is a polynomial shape of the third degree.
     */
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        throw new UnsupportedOperationException();
    }
}
