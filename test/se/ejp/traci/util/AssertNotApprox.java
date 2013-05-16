package se.ejp.traci.util;

import static org.junit.Assert.assertFalse;
import se.ejp.traci.math.Matrix;
import se.ejp.traci.math.Transformation;

public class AssertNotApprox
{
    public static void assertNotApprox(final Double d0, final Double d1)
    {
        assertFalse(ApproxEquals.approxEquals(d0, d1));
    }

    public static void assertNotApprox(final Double d0, final Double d1, final double epsilon)
    {
        assertFalse(ApproxEquals.approxEquals(d0, d1, epsilon));
    }

    public static void assertNotApprox(final Matrix m0, final Matrix m1)
    {
        assertFalse(ApproxEquals.approxEquals(m0, m1));
    }

    public static void assertNotApprox(final Matrix m0, final Matrix m1, final double epsilon)
    {
        assertFalse(ApproxEquals.approxEquals(m0, m1, epsilon));
    }

    public static void assertNotApprox(final Transformation t0, final Transformation t1)
    {
        assertFalse(ApproxEquals.approxEquals(t0, t1));
    }

    public static void assertNotApprox(final Transformation t0, final Transformation t1, final double epsilon)
    {
        assertFalse(ApproxEquals.approxEquals(t0, t1, epsilon));
    }
}
