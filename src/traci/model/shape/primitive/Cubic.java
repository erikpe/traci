package traci.model.shape.primitive;

import traci.math.Vector;
import traci.model.material.Material;
import traci.render.IntersectionStack;
import traci.render.Interval;
import traci.render.Ray;

public class Cubic extends Primitive
{
    public static final int NUM_COEFFS = 20;
    
    // private final double[] coeffs;
    
    public Cubic(final double ... coeffs)
    {
        this(null, coeffs);
    }
    
    public Cubic(final Material material, final double ... coeffs)
    {
        super(material);
        
        assert coeffs.length == NUM_COEFFS;
        // this.coeffs = Arrays.copyOf(coeffs, NUM_COEFFS);
    }
    
    @Override
    public Vector primitiveGetNormalAt(final Vector p)
    {
        throw new UnsupportedOperationException();
    }
    
    /**
     * A {@link Cubic} is a polynomial shape of the third degree.
     */
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        throw new UnsupportedOperationException();
    }
    
    @Override
    protected boolean primitiveIsInside(final Vector p)
    {
        throw new UnsupportedOperationException();
    }
    
    @Override
    protected boolean primitiveIsOutside(final Vector p)
    {
        throw new UnsupportedOperationException();
    }
    
    @Override
    protected void primitiveAllIntersections(final IntersectionStack iStack,
            final Vector p, final Vector dir)
    {
        final Ray ray = primitiveShootRay(p, dir);
        
        if (ray == null)
        {
            return;
        }
        
        for (final Interval ival : ray)
        {
            iStack.push(ival.p0().dist(), ival.p0().obj());
            
            if (ival.p1().dist() != ival.p0().dist())
            {
                iStack.push(ival.p1().dist(), ival.p1().obj());
            }
        }
    }
}
