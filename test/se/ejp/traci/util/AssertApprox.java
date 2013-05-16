package se.ejp.traci.util;

import static org.junit.Assert.assertTrue;
import se.ejp.traci.math.Matrix;
import se.ejp.traci.math.Transformation;

public class AssertApprox
{
    public static void assertApprox(final Double d0, final Double d1)
    {
        assertTrue(ApproxEquals.approxEquals(d0, d1));
    }

    public static void assertApprox(final Double d0, final Double d1, final double epsilon)
    {
        assertTrue(ApproxEquals.approxEquals(d0, d1, epsilon));
    }

    public static void assertApprox(final Matrix m0, final Matrix m1)
    {
        assertTrue(ApproxEquals.approxEquals(m0, m1));
    }

    public static void assertApprox(final Matrix m0, final Matrix m1, final double epsilon)
    {
        assertTrue(ApproxEquals.approxEquals(m0, m1, epsilon));
    }

    public static void assertApprox(final Transformation t0, final Transformation t1)
    {
        assertTrue(ApproxEquals.approxEquals(t0, t1));
    }

    public static void assertApprox(final Transformation t0, final Transformation t1, final double epsilon)
    {
        assertTrue(ApproxEquals.approxEquals(t0, t1, epsilon));
    }
}
