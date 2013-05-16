package se.ejp.traci.util;

import se.ejp.traci.math.Matrix;
import se.ejp.traci.math.Transformation;

public class ApproxEquals
{
    public static boolean approxEquals(final Double d0, final Double d1)
    {
        if (!d0.isInfinite() && !d1.isInfinite() && !d0.isNaN() && !d1.isNaN())
        {
            if (Math.abs(d0 - d1) <= Math.min(Math.ulp(d0), Math.ulp(d1)))
            {
                return true;
            }
        }

        return d0.equals(d1);
    }

    public static boolean approxEquals(final Double d0, final Double d1, final double epsilon)
    {
        if (!d0.isInfinite() && !d1.isInfinite() && !d0.isNaN() && !d1.isNaN())
        {
            if (Math.abs(d0 - d1) <= epsilon)
            {
                return true;
            }
        }

        return d0.equals(d1);
    }

    public static boolean approxEquals(final Matrix m0, final Matrix m1)
    {
        for (int row = 0; row < 4; ++row)
        {
            for (int col = 0; col < 4; ++col)
            {
                if (!approxEquals(m0.at(row, col), m1.at(row, col)))
                {
                    return false;
                }
            }
        }

        return true;
    }

    public static boolean approxEquals(final Matrix m0, final Matrix m1, final double epsilon)
    {
        for (int row = 0; row < 4; ++row)
        {
            for (int col = 0; col < 4; ++col)
            {
                if (!approxEquals(m0.at(row, col), m1.at(row, col), epsilon))
                {
                    return false;
                }
            }
        }

        return true;
    }

    public static boolean approxEquals(final Transformation t0, final Transformation t1)
    {
        return approxEquals(t0.mat, t1.mat) && approxEquals(t0.invMat, t1.invMat);
    }

    public static boolean approxEquals(final Transformation t0, final Transformation t1, final double epsilon)
    {
        return approxEquals(t0.mat, t1.mat, epsilon) && approxEquals(t0.invMat, t1.invMat, epsilon);
    }
}
