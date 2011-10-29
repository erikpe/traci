package traci.math;

import java.util.Arrays;

public class PolynomSolver
{
    private final static double SMALL_ENOUGH = 1.0e-10;
    private final static double TWO_M_PI_3 = 2.0943951023931954923084;
    private final static double FOUR_M_PI_3 = 4.1887902047863909846168;

    public static double[] solveQuadratic(final double[] coeffs)
    {
        final double a = coeffs[0];
        final double b = -coeffs[1];
        final double c = coeffs[2];

        if (a == 0.0)
        {
            if (b == 0.0)
            {
                return null;
            }

            return new double[] { c / b };
        }

        double d = b * b - 4.0 * a * c;

        /* Treat values of d around 0 as 0. */
        if (d > -SMALL_ENOUGH && d < SMALL_ENOUGH)
        {
            return new double[] { 0.5 * b / a };
        }
        else if (d < 0.0)
        {
            return null;
        }

        d = Math.sqrt(d);

        final double t = 2.0 * a;

        return new double[] { (b + d) / t, (b - d) / t };
    }

    public static double[] solveCubic(final double[] coeffs)
    {
        if (coeffs[0] == 0.0)
        {
            return solveQuadratic(new double[] { coeffs[1], coeffs[2],
                    coeffs[3] });
        }

        final double a1 = coeffs[1] / coeffs[0];
        final double a2 = coeffs[2] / coeffs[0];
        final double a3 = coeffs[3] / coeffs[0];

        final double A2 = a1 * a1;
        final double Q = (A2 - 3.0 * a2) / 9.0;
        final double R = (a1 * (A2 - 4.5 * a2) + 13.5 * a3) / 27.0;
        final double Q3 = Q * Q * Q;
        final double R2 = R * R;
        double d = Q3 - R2;
        final double an = a1 / 3.0;

        if (d >= 0.0)
        {
            /* Three real roots. */

            d = R / Math.sqrt(Q3);

            final double theta = Math.acos(d) / 3.0;

            final double sQ = -2.0 * Math.sqrt(Q);

            final double[] res = new double[3];

            res[0] = sQ * Math.cos(theta) - an;
            res[1] = sQ * Math.cos(theta + TWO_M_PI_3) - an;
            res[2] = sQ * Math.cos(theta + FOUR_M_PI_3) - an;

            return res;
        }

        final double sQ = Math.pow(Math.sqrt(R2 - Q3) + Math.abs(R), 1.0 / 3.0);

        final double[] res = new double[1];

        if (R < 0)
        {
            res[0] = (sQ + Q / sQ) - an;
        }
        else
        {
            res[0] = -(sQ + Q / sQ) - an;
        }

        return res;
    }

    public static double[] solveQuartic(final double[] coeffs)
    {
        final double c1 = coeffs[1] / coeffs[0];
        final double c2 = coeffs[2] / coeffs[0];
        final double c3 = coeffs[3] / coeffs[0];
        final double c4 = coeffs[4] / coeffs[0];

        final double c12 = c1 * c1;
        double p = -0.375 * c12 + c2;
        final double q = 0.125 * c12 * c1 - 0.5 * c1 * c2 + c3;
        final double r = -0.01171875 * c12 * c12 + 0.0625 * c12 * c2 - 0.25 * c1 * c3 + c4;

        final double[] cubic = new double[4];
        cubic[0] = 1.0;
        cubic[1] = -0.5 * p;
        cubic[2] = -r;
        cubic[3] = 0.5 * r * p - 0.125 * q * q;

        final double[] roots = solveCubic(cubic);
        int i = roots.length;

        double z;
        double d1, d2;

        if (i > 0)
        {
            z = roots[0];
        }
        else
        {
            return null;
        }

        d1 = 2.0 * z - p;

        if (d1 < 0.0)
        {
            if (d1 > -SMALL_ENOUGH)
            {
                d1 = 0.0;
            }
            else
            {
                return null;
            }
        }

        if (d1 < SMALL_ENOUGH)
        {
            d2 = z * z - r;

            if (d2 < 0.0)
            {
                return null;
            }

            d2 = Math.sqrt(d2);
        }
        else
        {
            d1 = Math.sqrt(d1);
            d2 = 0.5 * q / d1;
        }

        /* Set up useful values for the quadratic factors */

        final double q1 = d1 * d1;
        final double q2 = -0.25 * c1;

        final double[] results = new double[4];
        i = 0;

        /* Solve the first quadratic */

        p = q1 - 4.0 * (z - d2);

        if (p == 0)
        {
            //results[i++] = -0.5 * d1 - q2;
        }
        else
        {
            if (p > 0)
            {
                p = Math.sqrt(p);
                results[i++] = -0.5 * (d1 + p) + q2;
                results[i++] = -0.5 * (d1 - p) + q2;
            }
        }

        /* Solve the second quadratic */

        p = q1 - 4.0 * (z + d2);

        if (p == 0)
        {
            //results[i++] = 0.5 * d1 - q2;
        }
        else
        {
            if (p > 0)
            {
                p = Math.sqrt(p);
                results[i++] = 0.5 * (d1 + p) + q2;
                results[i++] = 0.5 * (d1 - p) + q2;
            }
        }

        if (i > 0)
        {
            return Arrays.copyOfRange(results, 0, i);
        }

        return null;
    }
}
