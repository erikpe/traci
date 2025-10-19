package se.ejp.traci.math;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class Vector2DTest
{
    private static final double EPSILON = 1e-10;

    @Test
    public void testMake()
    {
        final Vector2D v = Vector2D.make(1.5, 2.5);
        assertNotNull(v);
    }

    @Test
    public void testMakeWithZeros()
    {
        final Vector2D v = Vector2D.make(0.0, 0.0);
        assertNotNull(v);
        assertEquals(0.0, v.x(), 0);
        assertEquals(0.0, v.y(), 0);
    }

    @Test
    public void testMakeWithNegative()
    {
        final Vector2D v = Vector2D.make(-3.5, -7.2);
        assertNotNull(v);
        assertEquals(-3.5, v.x(), 0);
        assertEquals(-7.2, v.y(), 0);
    }

    @Test
    public void testX()
    {
        final Vector2D v = Vector2D.make(3.14, 2.71);
        assertEquals(3.14, v.x(), EPSILON);
    }

    @Test
    public void testY()
    {
        final Vector2D v = Vector2D.make(3.14, 2.71);
        assertEquals(2.71, v.y(), EPSILON);
    }

    @Test
    public void testXPositive()
    {
        final Vector2D v = Vector2D.make(42.0, 0.0);
        assertEquals(42.0, v.x(), 0);
    }

    @Test
    public void testYPositive()
    {
        final Vector2D v = Vector2D.make(0.0, 17.5);
        assertEquals(17.5, v.y(), 0);
    }

    @Test
    public void testXNegative()
    {
        final Vector2D v = Vector2D.make(-5.5, 0.0);
        assertEquals(-5.5, v.x(), 0);
    }

    @Test
    public void testYNegative()
    {
        final Vector2D v = Vector2D.make(0.0, -8.3);
        assertEquals(-8.3, v.y(), 0);
    }

    @Test
    public void testXZero()
    {
        final Vector2D v = Vector2D.make(0.0, 5.0);
        assertEquals(0.0, v.x(), 0);
    }

    @Test
    public void testYZero()
    {
        final Vector2D v = Vector2D.make(5.0, 0.0);
        assertEquals(0.0, v.y(), 0);
    }

    @Test
    public void testLargeValues()
    {
        final double largeX = 1e10;
        final double largeY = 1e11;
        final Vector2D v = Vector2D.make(largeX, largeY);
        
        assertEquals(largeX, v.x(), 0);
        assertEquals(largeY, v.y(), 0);
    }

    @Test
    public void testSmallValues()
    {
        final double smallX = 1e-10;
        final double smallY = 1e-11;
        final Vector2D v = Vector2D.make(smallX, smallY);
        
        assertEquals(smallX, v.x(), 0);
        assertEquals(smallY, v.y(), 0);
    }

    @Test
    public void testMultipleInstances()
    {
        final Vector2D v1 = Vector2D.make(1.0, 2.0);
        final Vector2D v2 = Vector2D.make(3.0, 4.0);
        final Vector2D v3 = Vector2D.make(5.0, 6.0);
        
        assertEquals(1.0, v1.x(), 0);
        assertEquals(2.0, v1.y(), 0);
        assertEquals(3.0, v2.x(), 0);
        assertEquals(4.0, v2.y(), 0);
        assertEquals(5.0, v3.x(), 0);
        assertEquals(6.0, v3.y(), 0);
    }

    @Test
    public void testImmutability()
    {
        final Vector2D v = Vector2D.make(1.0, 2.0);
        final double originalX = v.x();
        final double originalY = v.y();
        
        // Call getters multiple times
        v.x();
        v.y();
        v.x();
        v.y();
        
        // Values should remain the same
        assertEquals(originalX, v.x(), 0);
        assertEquals(originalY, v.y(), 0);
    }

    @Test
    public void testSameValuesDifferentInstances()
    {
        final Vector2D v1 = Vector2D.make(1.0, 2.0);
        final Vector2D v2 = Vector2D.make(1.0, 2.0);
        
        // Should have same values
        assertEquals(v1.x(), v2.x(), 0);
        assertEquals(v1.y(), v2.y(), 0);
        
        // But be different instances
        assertFalse(v1 == v2);
    }

    @Test
    public void testSpecialValuesInfinity()
    {
        final Vector2D v1 = Vector2D.make(Double.POSITIVE_INFINITY, 0.0);
        final Vector2D v2 = Vector2D.make(0.0, Double.NEGATIVE_INFINITY);
        final Vector2D v3 = Vector2D.make(Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY);
        
        assertTrue(Double.isInfinite(v1.x()));
        assertTrue(Double.isInfinite(v2.y()));
        assertTrue(Double.isInfinite(v3.x()));
        assertTrue(Double.isInfinite(v3.y()));
    }

    @Test
    public void testSpecialValuesNaN()
    {
        final Vector2D v = Vector2D.make(Double.NaN, Double.NaN);
        
        assertTrue(Double.isNaN(v.x()));
        assertTrue(Double.isNaN(v.y()));
    }

    @Test
    public void testMaxMinValues()
    {
        final Vector2D v = Vector2D.make(Double.MAX_VALUE, Double.MIN_VALUE);
        
        assertEquals(Double.MAX_VALUE, v.x(), 0);
        assertEquals(Double.MIN_VALUE, v.y(), 0);
    }

    @Test
    public void testUnitVectorX()
    {
        final Vector2D v = Vector2D.make(1.0, 0.0);
        assertEquals(1.0, v.x(), 0);
        assertEquals(0.0, v.y(), 0);
    }

    @Test
    public void testUnitVectorY()
    {
        final Vector2D v = Vector2D.make(0.0, 1.0);
        assertEquals(0.0, v.x(), 0);
        assertEquals(1.0, v.y(), 0);
    }

    @Test
    public void testNegativeUnitVectorX()
    {
        final Vector2D v = Vector2D.make(-1.0, 0.0);
        assertEquals(-1.0, v.x(), 0);
        assertEquals(0.0, v.y(), 0);
    }

    @Test
    public void testNegativeUnitVectorY()
    {
        final Vector2D v = Vector2D.make(0.0, -1.0);
        assertEquals(0.0, v.x(), 0);
        assertEquals(-1.0, v.y(), 0);
    }

    @Test
    public void testFractionalValues()
    {
        final Vector2D v = Vector2D.make(0.123456789, 0.987654321);
        assertEquals(0.123456789, v.x(), EPSILON);
        assertEquals(0.987654321, v.y(), EPSILON);
    }

    @Test
    public void testPrecision()
    {
        final double x = 1.0 / 3.0;
        final double y = 2.0 / 3.0;
        final Vector2D v = Vector2D.make(x, y);
        
        assertEquals(x, v.x(), 0);
        assertEquals(y, v.y(), 0);
    }

    @Test
    public void testMixedSigns()
    {
        final Vector2D v1 = Vector2D.make(1.0, -1.0);
        final Vector2D v2 = Vector2D.make(-1.0, 1.0);
        
        assertEquals(1.0, v1.x(), 0);
        assertEquals(-1.0, v1.y(), 0);
        assertEquals(-1.0, v2.x(), 0);
        assertEquals(1.0, v2.y(), 0);
    }

    @Test
    public void testVerySmallDifferences()
    {
        final double x1 = 1.0;
        final double x2 = 1.0 + 1e-15;
        final Vector2D v1 = Vector2D.make(x1, 0.0);
        final Vector2D v2 = Vector2D.make(x2, 0.0);
        
        // Vectors should preserve the exact values
        assertEquals(x1, v1.x(), 0);
        assertEquals(x2, v2.x(), 0);
    }

    @Test
    public void testRationalApproximations()
    {
        // Test common rational approximations
        final Vector2D v1 = Vector2D.make(22.0 / 7.0, 0.0); // Pi approximation
        final Vector2D v2 = Vector2D.make(0.0, 355.0 / 113.0); // Better Pi approximation
        
        assertEquals(22.0 / 7.0, v1.x(), EPSILON);
        assertEquals(355.0 / 113.0, v2.y(), EPSILON);
    }

    @Test
    public void testSymmetricValues()
    {
        final Vector2D v1 = Vector2D.make(5.0, 5.0);
        final Vector2D v2 = Vector2D.make(-5.0, -5.0);
        
        assertEquals(5.0, v1.x(), 0);
        assertEquals(5.0, v1.y(), 0);
        assertEquals(-5.0, v2.x(), 0);
        assertEquals(-5.0, v2.y(), 0);
    }

    @Test
    public void testGettersMultipleCalls()
    {
        final Vector2D v = Vector2D.make(Math.PI, Math.E);
        
        // Call getters multiple times to ensure consistency
        for (int i = 0; i < 100; i++)
        {
            assertEquals(Math.PI, v.x(), EPSILON);
            assertEquals(Math.E, v.y(), EPSILON);
        }
    }

    @Test
    public void testManyVectors()
    {
        // Create and verify many vectors
        for (int i = 0; i < 100; i++)
        {
            final double x = i * 0.5;
            final double y = i * 1.5;
            final Vector2D v = Vector2D.make(x, y);
            
            assertEquals(x, v.x(), EPSILON);
            assertEquals(y, v.y(), EPSILON);
        }
    }

    @Test
    public void testEqualsIdentity()
    {
        final Vector2D v = Vector2D.make(1.0, 2.0);
        assertTrue(v.equals(v));
    }

    @Test
    public void testEqualsEqualVectors()
    {
        final Vector2D v1 = Vector2D.make(1.0, 2.0);
        final Vector2D v2 = Vector2D.make(1.0, 2.0);
        assertTrue(v1.equals(v2));
        assertTrue(v2.equals(v1));
    }

    @Test
    public void testEqualsDifferentX()
    {
        final Vector2D v1 = Vector2D.make(1.0, 2.0);
        final Vector2D v2 = Vector2D.make(1.1, 2.0);
        assertFalse(v1.equals(v2));
    }

    @Test
    public void testEqualsDifferentY()
    {
        final Vector2D v1 = Vector2D.make(1.0, 2.0);
        final Vector2D v2 = Vector2D.make(1.0, 2.1);
        assertFalse(v1.equals(v2));
    }

    @Test
    public void testEqualsNull()
    {
        final Vector2D v = Vector2D.make(1.0, 2.0);
        assertFalse(v.equals(null));
    }

    @Test
    public void testEqualsDifferentType()
    {
        final Vector2D v = Vector2D.make(1.0, 2.0);
        assertFalse(v.equals("not a vector"));
    }

    @Test
    public void testEqualsZeroAndNegativeZero()
    {
        final Vector2D v1 = Vector2D.make(0.0, 0.0);
        final Vector2D v2 = Vector2D.make(-0.0, -0.0);
        // Double.valueOf(0.0) != Double.valueOf(-0.0) due to different bit patterns
        assertFalse(v1.equals(v2));
    }

    @Test
    public void testEqualsNaN()
    {
        final Vector2D v1 = Vector2D.make(Double.NaN, 1.0);
        final Vector2D v2 = Vector2D.make(Double.NaN, 1.0);
        // Double.valueOf(NaN).equals(Double.valueOf(NaN)) is true
        assertTrue(v1.equals(v2));
    }

    @Test
    public void testHashCodeConsistent()
    {
        final Vector2D v = Vector2D.make(1.0, 2.0);
        assertEquals(v.hashCode(), v.hashCode());
    }

    @Test
    public void testHashCodeEqualVectorsHaveEqualHashCodes()
    {
        final Vector2D v1 = Vector2D.make(1.0, 2.0);
        final Vector2D v2 = Vector2D.make(1.0, 2.0);
        assertEquals(v1.hashCode(), v2.hashCode());
    }

    @Test
    public void testHashCodeDifferentVectors()
    {
        final Vector2D v1 = Vector2D.make(1.0, 2.0);
        final Vector2D v2 = Vector2D.make(2.0, 1.0);
        // Different vectors may have different hash codes (not required, but likely)
        assertFalse(v1.hashCode() == v2.hashCode());
    }

    @Test
    public void testHashCodeZeroAndNegativeZero()
    {
        final Vector2D v1 = Vector2D.make(0.0, 0.0);
        final Vector2D v2 = Vector2D.make(-0.0, -0.0);
        // Double.hashCode treats 0.0 and -0.0 as having the same hash code
        // even though Double.equals treats them as different
        assertEquals(v1.hashCode(), v2.hashCode());
    }
}
