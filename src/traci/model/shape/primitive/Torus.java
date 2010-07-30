package traci.model.shape.primitive;

import traci.math.PolynomSolver;
import traci.math.Vector;
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
    
    private Point hitPoint(final Vector p, final Vector dir, final double t)
    {
        final Vector hit = p.add(dir.mul(t));
        final double x = hit.x();
        final double y = hit.y();
        final double z = hit.z();
        
        final double k = x * x + y * y + z * z - r * r - 1;
        final Vector normal = Vector.make(4 * x * k, 4 * y * k, 4 * z * k + 8 * z).normalize();
        
        return Point.make(t, this, normal);
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
            
            final Point nearP = hitPoint(p, dir, near);
            final Point farP = hitPoint(p, dir, far);
            
            ray = new Ray(Interval.make(nearP, farP));
        }
        
//        if (roots.length == 4)
//        {
//            ray.merge(new Ray(Interval.make(hitPoint(p, dir, roots[2]),
//                    hitPoint(p, dir, roots[3]))));
//        }
        
        return ray;
    }
}
