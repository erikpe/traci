package traci.model.shape.primitive;

import java.util.Arrays;

import traci.math.PolynomSolver;
import traci.math.Transformations;
import traci.math.Vector;
import traci.render.Point.Type;
import traci.render.Ray;

public class Torus extends Primitive
{
    final double r;
    final double r2;

    public Torus(final double r, final double R)
    {
        this.r = r / R;
        this.r2 = this.r * this.r;

        transform(Transformations.scale(R));
    }

    @Override
    public Vector primitiveGetNormalAt(final Vector p)
    {
        final double x = p.x();
        final double y = p.y();
        final double z = p.z();

        final double k = x * x + y * y + z * z - r2 - 1;

        return Vector.make(4 * x * k, 4 * y * k, 4 * z * k + 8 * z);
    }

    private Double move(final Vector p, final Vector dir)
    {
        final double radius = 1 + r;

        final double c = dir.dot(dir);
        final double a = 2 * p.dot(dir) / c;
        final double b = (p.dot(p) - radius * radius) / c;

        final double d = (a * a) / 4 - b;

        if (d >= 0)
        {
            final double t = -a / 2 - Math.sqrt(d);

            if (t < 0)
            {
                return Double.valueOf(0);
            }

            return t;
        }

        return null;
    }

    /**
     * The torus lies in the xz-plane, has a major radius of {@code 1}, and a
     * minor radius of {@code r}.
     *
     * It is a special case of a {@link Quartic} surface.
     */
    @Override
    public Ray primitiveShootRay(final Vector initP, final Vector dir)
    {
        final Double move = move(initP, dir);

        if (move == null)
        {
            return null;
        }

        final Vector p = initP.add(dir.mul(move));

        final double a = dir.dot(dir);
        final double b = 2 * p.dot(dir);
        final double g = p.dot(p) - r2 - 1;

        final double a4 = a * a;
        final double a3 = 2 * a * b;
        final double a2 = b * b + 2 * a * g + 4 * dir.z() * dir.z();
        final double a1 = 2 * b * g + 8 * p.z() * dir.z();
        final double a0 = g * g + 4 * p.z() * p.z() - 4 * r2;

        final double[] roots = PolynomSolver.solveQuartic(new double[] { a4, a3, a2, a1, a0 });

        if (roots == null || (roots.length != 2 && roots.length != 4))
        {
            return null;
        }

        Ray ray = null;

        Arrays.sort(roots);

        if (roots[1] > -0.001)
        {
            ray = Ray.make();

            ray.add(roots[0] + move, this, Type.ENTER);
            ray.add(roots[1] + move, this, Type.LEAVE);
        }

        if (roots.length == 4 && roots[3] > -0.001)
        {
            if (ray == null)
            {
                ray = Ray.make();
            }

            ray.add(roots[2] + move, this, Type.ENTER);
            ray.add(roots[3] + move, this, Type.LEAVE);
        }

        return ray;
    }
}
