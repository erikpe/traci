package traci.model.csg;

import java.util.Arrays;

import traci.math.Vector;
import traci.model.material.Material;
import traci.render.Ray;

public class Quadric extends Primitive
{
    public static final int NUM_COEFFS = 10;
    
    private final double[] coeffs;
    
    public Quadric(final double ... coeffs)
    {
        this(null, coeffs);
    }
    
    public Quadric(final Material material, final double ... coeffs)
    {
        super(material);
        
        assert coeffs.length == NUM_COEFFS;
        this.coeffs = Arrays.copyOf(coeffs, NUM_COEFFS);
    }
    
    /**
     * A quadric is a polynomial shape of the second degree. It is defined by
     * the equation
     * 
     * {@code A*x^2 + B*y^2 + C*z^2 + D*xy + E*xz + F*yz + G*x + H*y + I*z + J = 0}
     * 
     * where {@code coeffs} consists of the coefficients
     * {@code { A, B, C, D ... }}
     */
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        throw new UnsupportedOperationException();
    }
}
