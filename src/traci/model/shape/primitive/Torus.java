package traci.model.shape.primitive;

import traci.math.PolynomSolver;
import traci.math.Vector;
import traci.render.IntersectionStack;
import traci.render.Interval;
import traci.render.Point;
import traci.render.Ray;

public class Torus extends Primitive
{
    final double r;
    
    public Torus(final double r)
    {
        this(r, 1);
    }
    
    public Torus(final double r, final double R)
    {
        super(null);
        
        this.r = r / R;
        scale(R);
    }
    
    @Override
    public Vector primitiveGetNormalAt(final Vector p)
    {
        final double x = p.x();
        final double y = p.y();
        final double z = p.z();
        
        final double k = x * x + y * y + z * z + r * r - 1;
        
        return Vector.make(4 * x * k, 4 * y * k, 4 * z * k + 8 * z);
    }
    
    /**
     * The torus lies in the xz-plane, has a major radius of {@code 1}, and a
     * minor radius of {@code r}.
     * 
     * It is a special case of a {@link Quartic} surface.
     */
    @Deprecated
    @Override
    public Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        final double a = dir.dot(dir);
        final double b = 2 * p.dot(dir);
        final double g = p.dot(p) - r * r - 1;
        
        final double a4 = a * a;
        final double a3 = 2 * a * b;
        final double a2 = b * b + 2 * a * g + 4 * dir.z() * dir.z();
        final double a1 = 2 * b * g + 8 * p.z() * dir.z();
        final double a0 = g * g + 4 * p.z() * p.z() - 4 * r * r;
        
        final double[] roots = PolynomSolver.solveQuartic(new double[] { a4,
                a3, a2, a1, a0 });
        
        if (roots == null)
        {
            return null;
        }
        
        Ray ray = null;
        
        if (roots.length == 2 || roots.length == 4)
        {
            final double near = Math.min(roots[0], roots[1]);
            final double far = Math.max(roots[0], roots[1]);
            
            if (near < EPSILON)
            {
                return null;
            }
            
            ray = new Ray(Interval.make(Point.make(near, this), Point.make(far, this)));
        }
        
        return ray;
    }
    
    @Override
    protected boolean primitiveIsInside(final Vector p)
    {
        final double r2 = r * r;
        final double a = p.dot(p) - r2 - 1;
        final double b = a * a + 4 * (p.z() * p.z() - r2);
        
        return b < INSIDE_MARIGIN;
    }
    
    @Override
    protected boolean primitiveIsOutside(final Vector p)
    {
        final double r2 = r * r;
        final double a = p.dot(p) - r2 - 1;
        final double b = a * a + 4 * (p.z() * p.z() - r2);
        
        return b > -INSIDE_MARIGIN;
    }
    
    @Override
    protected void primitiveAllIntersections(final IntersectionStack iStack,
            final Vector p, final Vector dir)
    {
        final double a = dir.dot(dir);
        final double b = 2 * p.dot(dir);
        final double g = p.dot(p) - r * r - 1;
        
        final double a4 = a * a;
        final double a3 = 2 * a * b;
        final double a2 = b * b + 2 * a * g + 4 * dir.z() * dir.z();
        final double a1 = 2 * b * g + 8 * p.z() * dir.z();
        final double a0 = g * g + 4 * p.z() * p.z() - 4 * r * r;
        
        final double[] roots = PolynomSolver.solveQuartic(new double[] { a4,
                a3, a2, a1, a0 });
        
        if (roots == null)
        {
            return;
        }
        
        for (int i = 0; i < roots.length; ++i)
        {
            if (roots[i] > EPSILON)
            {
                iStack.push(roots[i], this);
            }
        }
        
//        final Ray ray = primitiveShootRay(p, dir);
//        
//        if (ray == null)
//        {
//            return;
//        }
//        
//        for (final Interval ival : ray)
//        {
//            iStack.push(ival.p0().dist(), ival.p0().obj());
//            
//            if (ival.p1().dist() > ival.p0().dist())
//            {
//                iStack.push(ival.p1().dist(), ival.p1().obj());
//            }
//        }
    }
}
