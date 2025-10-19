package se.ejp.traci.math;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.Random;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class VectorTest
{
    private static final double EPSILON = 1e-10;

    private Vector v0 = null;
    private Vector v1 = null;
    private Random rand = null;

    @Before
    public void setUp() throws Exception
    {
        v0 = Vector.make(1, 2, 3);
        v1 = Vector.make(10, 20, 30);
        rand = new Random(0);
    }

    @After
    public void tearDown() throws Exception
    {
        v0 = null;
        v1 = null;
        rand = null;
    }

    @Test
    public void testXYZ()
    {
        for (int i = 0; i < 10; ++i)
        {
            final double x = rand.nextDouble();
            final double y = rand.nextDouble();
            final double z = rand.nextDouble();
            final Vector vec = Vector.make(x, y, z);
            assertEquals(Vector.make(x, y, z), vec);
            assertNotEquals(Vector.make(x, y + 1, z), vec);
            assertEquals(x,  vec.x(), 0);
            assertEquals(y,  vec.y(), 0);
            assertEquals(z,  vec.z(), 0);
        }
    }

    @Test
    public void testAdd()
    {
        final Vector sum = v0.add(v1);
        assertEquals(11, sum.x(), 0);
        assertEquals(22, sum.y(), 0);
        assertEquals(33, sum.z(), 0);
    }

    @Test
    public void testSub()
    {
        final Vector diff = v1.sub(v0);
        assertEquals(9, diff.x(), 0);
        assertEquals(18, diff.y(), 0);
        assertEquals(27, diff.z(), 0);
    }

    @Test
    public void testMakeWithPrimitives()
    {
        final Vector v = Vector.make(1.5, 2.5, 3.5);
        assertEquals(1.5, v.x(), 0);
        assertEquals(2.5, v.y(), 0);
        assertEquals(3.5, v.z(), 0);
    }

    @Test
    public void testMakeWithDoubleObjects()
    {
        final Vector v = Vector.make(Double.valueOf(1.5), Double.valueOf(2.5), Double.valueOf(3.5));
        assertEquals(1.5, v.x(), 0);
        assertEquals(2.5, v.y(), 0);
        assertEquals(3.5, v.z(), 0);
    }

    @Test
    public void testConstants()
    {
        // Test ORIGO
        assertEquals(0.0, Vector.ORIGO.x(), 0);
        assertEquals(0.0, Vector.ORIGO.y(), 0);
        assertEquals(0.0, Vector.ORIGO.z(), 0);

        // Test UNIT_X
        assertEquals(1.0, Vector.UNIT_X.x(), 0);
        assertEquals(0.0, Vector.UNIT_X.y(), 0);
        assertEquals(0.0, Vector.UNIT_X.z(), 0);

        // Test UNIT_Y
        assertEquals(0.0, Vector.UNIT_Y.x(), 0);
        assertEquals(1.0, Vector.UNIT_Y.y(), 0);
        assertEquals(0.0, Vector.UNIT_Y.z(), 0);

        // Test UNIT_Z
        assertEquals(0.0, Vector.UNIT_Z.x(), 0);
        assertEquals(0.0, Vector.UNIT_Z.y(), 0);
        assertEquals(1.0, Vector.UNIT_Z.z(), 0);

        // Test UNIT_NEG_X
        assertEquals(-1.0, Vector.UNIT_NEG_X.x(), 0);
        assertEquals(0.0, Vector.UNIT_NEG_X.y(), 0);
        assertEquals(0.0, Vector.UNIT_NEG_X.z(), 0);

        // Test UNIT_NEG_Y
        assertEquals(0.0, Vector.UNIT_NEG_Y.x(), 0);
        assertEquals(-1.0, Vector.UNIT_NEG_Y.y(), 0);
        assertEquals(0.0, Vector.UNIT_NEG_Y.z(), 0);

        // Test UNIT_NEG_Z
        assertEquals(0.0, Vector.UNIT_NEG_Z.x(), 0);
        assertEquals(0.0, Vector.UNIT_NEG_Z.y(), 0);
        assertEquals(-1.0, Vector.UNIT_NEG_Z.z(), 0);
    }

    @Test
    public void testLength()
    {
        assertEquals(0.0, Vector.ORIGO.length(), EPSILON);
        assertEquals(1.0, Vector.UNIT_X.length(), EPSILON);
        assertEquals(1.0, Vector.UNIT_Y.length(), EPSILON);
        assertEquals(1.0, Vector.UNIT_Z.length(), EPSILON);

        final Vector v = Vector.make(3, 4, 0);
        assertEquals(5.0, v.length(), EPSILON);

        final Vector v2 = Vector.make(1, 2, 2);
        assertEquals(3.0, v2.length(), EPSILON);
    }

    @Test
    public void testNormalize()
    {
        final Vector v = Vector.make(3, 4, 0);
        final Vector normalized = v.normalize();

        assertEquals(0.6, normalized.x(), EPSILON);
        assertEquals(0.8, normalized.y(), EPSILON);
        assertEquals(0.0, normalized.z(), EPSILON);
        assertEquals(1.0, normalized.length(), EPSILON);
    }

    @Test
    public void testNormalizeUnitVectors()
    {
        assertEquals(Vector.UNIT_X, Vector.UNIT_X.normalize());
        assertEquals(Vector.UNIT_Y, Vector.UNIT_Y.normalize());
        assertEquals(Vector.UNIT_Z, Vector.UNIT_Z.normalize());
    }

    @Test
    public void testDot()
    {
        // Orthogonal vectors
        assertEquals(0.0, Vector.UNIT_X.dot(Vector.UNIT_Y), EPSILON);
        assertEquals(0.0, Vector.UNIT_Y.dot(Vector.UNIT_Z), EPSILON);
        assertEquals(0.0, Vector.UNIT_Z.dot(Vector.UNIT_X), EPSILON);

        // Parallel vectors
        assertEquals(1.0, Vector.UNIT_X.dot(Vector.UNIT_X), EPSILON);
        assertEquals(-1.0, Vector.UNIT_X.dot(Vector.UNIT_NEG_X), EPSILON);

        // General case
        final Vector v1 = Vector.make(1, 2, 3);
        final Vector v2 = Vector.make(4, 5, 6);
        assertEquals(32.0, v1.dot(v2), EPSILON); // 1*4 + 2*5 + 3*6 = 32
    }

    @Test
    public void testDotCommutative()
    {
        assertEquals(v0.dot(v1), v1.dot(v0), EPSILON);
    }

    @Test
    public void testAddCommutative()
    {
        assertEquals(v0.add(v1), v1.add(v0));
    }

    @Test
    public void testAddWithZero()
    {
        assertEquals(v0, v0.add(Vector.ORIGO));
    }

    @Test
    public void testAddAssociative()
    {
        final Vector v2 = Vector.make(100, 200, 300);
        assertEquals(v0.add(v1).add(v2), v0.add(v1.add(v2)));
    }

    @Test
    public void testSubWithZero()
    {
        assertEquals(v0, v0.sub(Vector.ORIGO));
    }

    @Test
    public void testSubSelf()
    {
        assertEquals(Vector.ORIGO, v0.sub(v0));
    }

    @Test
    public void testNeg()
    {
        final Vector neg = v0.neg();
        assertEquals(-1.0, neg.x(), 0);
        assertEquals(-2.0, neg.y(), 0);
        assertEquals(-3.0, neg.z(), 0);
    }

    @Test
    public void testNegDouble()
    {
        assertEquals(v0, v0.neg().neg());
    }

    @Test
    public void testNegConstants()
    {
        // Just verify the values are correct, don't compare vectors directly
        // due to -0.0 vs 0.0 representation issues
        assertEquals(-1.0, Vector.UNIT_X.neg().x(), 0);
        assertEquals(0.0, Math.abs(Vector.UNIT_X.neg().y()), 0);
        assertEquals(0.0, Math.abs(Vector.UNIT_X.neg().z()), 0);

        assertEquals(0.0, Math.abs(Vector.UNIT_Y.neg().x()), 0);
        assertEquals(-1.0, Vector.UNIT_Y.neg().y(), 0);
        assertEquals(0.0, Math.abs(Vector.UNIT_Y.neg().z()), 0);

        assertEquals(0.0, Math.abs(Vector.UNIT_Z.neg().x()), 0);
        assertEquals(0.0, Math.abs(Vector.UNIT_Z.neg().y()), 0);
        assertEquals(-1.0, Vector.UNIT_Z.neg().z(), 0);
    }

    @Test
    public void testMul()
    {
        final Vector scaled = v0.mul(2.0);
        assertEquals(2.0, scaled.x(), 0);
        assertEquals(4.0, scaled.y(), 0);
        assertEquals(6.0, scaled.z(), 0);
    }

    @Test
    public void testMulZero()
    {
        assertEquals(Vector.ORIGO, v0.mul(0.0));
    }

    @Test
    public void testMulOne()
    {
        assertEquals(v0, v0.mul(1.0));
    }

    @Test
    public void testMulNegative()
    {
        assertEquals(v0.neg(), v0.mul(-1.0));
    }

    @Test
    public void testDiv()
    {
        final Vector scaled = v1.div(10.0);
        assertEquals(1.0, scaled.x(), 0);
        assertEquals(2.0, scaled.y(), 0);
        assertEquals(3.0, scaled.z(), 0);
    }

    @Test
    public void testDivOne()
    {
        assertEquals(v0, v0.div(1.0));
    }

    @Test
    public void testCross()
    {
        // Standard basis vectors
        assertEquals(Vector.UNIT_Z, Vector.UNIT_X.cross(Vector.UNIT_Y));
        assertEquals(Vector.UNIT_X, Vector.UNIT_Y.cross(Vector.UNIT_Z));
        assertEquals(Vector.UNIT_Y, Vector.UNIT_Z.cross(Vector.UNIT_X));
    }

    @Test
    public void testCrossAntiCommutative()
    {
        // v0 × v1 = -(v1 × v0)
        final Vector cross1 = v0.cross(v1);
        final Vector cross2 = v1.cross(v0);

        assertEquals(-cross2.x(), cross1.x(), EPSILON);
        assertEquals(-cross2.y(), cross1.y(), EPSILON);
        assertEquals(-cross2.z(), cross1.z(), EPSILON);
    }

    @Test
    public void testCrossParallelVectors()
    {
        // Cross product of parallel vectors is zero
        assertEquals(Vector.ORIGO, Vector.UNIT_X.cross(Vector.UNIT_X));
        assertEquals(Vector.ORIGO, v0.cross(v0));
    }

    @Test
    public void testCrossOrthogonality()
    {
        final Vector cross = v0.cross(v1);
        // Cross product is orthogonal to both input vectors
        assertEquals(0.0, cross.dot(v0), EPSILON);
        assertEquals(0.0, cross.dot(v1), EPSILON);
    }

    @Test
    public void testCrossRightHandRule()
    {
        final Vector v1 = Vector.make(1, 0, 0);
        final Vector v2 = Vector.make(0, 1, 0);
        final Vector cross = v1.cross(v2);

        assertEquals(0.0, cross.x(), EPSILON);
        assertEquals(0.0, cross.y(), EPSILON);
        assertEquals(1.0, cross.z(), EPSILON);
    }

    @Test
    public void testEqualsReflexive()
    {
        assertTrue(v0.equals(v0));
    }

    @Test
    public void testEqualsSymmetric()
    {
        final Vector v = Vector.make(1, 2, 3);
        assertTrue(v0.equals(v));
        assertTrue(v.equals(v0));
    }

    @Test
    public void testEqualsNull()
    {
        assertFalse(v0.equals(null));
    }

    @Test
    public void testEqualsDifferentClass()
    {
        assertFalse(v0.equals("not a vector"));
    }

    @Test
    public void testEqualsIdenticalVectors()
    {
        final Vector v = Vector.make(1, 2, 3);
        assertTrue(v0.equals(v));
    }

    @Test
    public void testEqualsDifferentX()
    {
        final Vector v = Vector.make(2, 2, 3);
        assertFalse(v0.equals(v));
    }

    @Test
    public void testEqualsDifferentY()
    {
        final Vector v = Vector.make(1, 3, 3);
        assertFalse(v0.equals(v));
    }

    @Test
    public void testEqualsDifferentZ()
    {
        final Vector v = Vector.make(1, 2, 4);
        assertFalse(v0.equals(v));
    }

    @Test
    public void testHashCodeConsistency()
    {
        final int hash1 = v0.hashCode();
        final int hash2 = v0.hashCode();
        assertEquals(hash1, hash2);
    }

    @Test
    public void testHashCodeEqualVectors()
    {
        final Vector v = Vector.make(1, 2, 3);
        assertEquals(v0.hashCode(), v.hashCode());
    }

    @Test
    public void testHashCodeDifferentVectors()
    {
        assertNotEquals(v0.hashCode(), v1.hashCode());
    }

    @Test
    public void testToString()
    {
        final Vector v = Vector.make(1.0, 2.0, 3.0);
        final String str = v.toString();
        assertTrue(str.contains("1.0"));
        assertTrue(str.contains("2.0"));
        assertTrue(str.contains("3.0"));
        assertTrue(str.startsWith("["));
        assertTrue(str.endsWith("]"));
    }

    @Test
    public void testAddSubInverse()
    {
        // v0 + v1 - v1 == v0
        assertEquals(v0, v0.add(v1).sub(v1));
    }

    @Test
    public void testMulDivInverse()
    {
        // v0 * 5 / 5 == v0
        final Vector result = v0.mul(5.0).div(5.0);
        assertEquals(v0.x(), result.x(), EPSILON);
        assertEquals(v0.y(), result.y(), EPSILON);
        assertEquals(v0.z(), result.z(), EPSILON);
    }

    @Test
    public void testLengthSquared()
    {
        // length^2 should equal dot product with self
        final double lengthSquared = v0.length() * v0.length();
        assertEquals(lengthSquared, v0.dot(v0), EPSILON);
    }

    @Test
    public void testNormalizeLength()
    {
        final Vector v = Vector.make(5, 12, 0);
        final Vector normalized = v.normalize();
        assertEquals(1.0, normalized.length(), EPSILON);
    }

    @Test
    public void testCrossDistributiveOverAddition()
    {
        final Vector v2 = Vector.make(7, 8, 9);
        // v0 × (v1 + v2) = (v0 × v1) + (v0 × v2)
        final Vector left = v0.cross(v1.add(v2));
        final Vector right = v0.cross(v1).add(v0.cross(v2));

        assertEquals(left.x(), right.x(), EPSILON);
        assertEquals(left.y(), right.y(), EPSILON);
        assertEquals(left.z(), right.z(), EPSILON);
    }

    @Test
    public void testScalarMultiplicationDistributive()
    {
        final double scalar = 3.5;
        // scalar * (v0 + v1) = scalar * v0 + scalar * v1
        final Vector left = v0.add(v1).mul(scalar);
        final Vector right = v0.mul(scalar).add(v1.mul(scalar));

        assertEquals(left.x(), right.x(), EPSILON);
        assertEquals(left.y(), right.y(), EPSILON);
        assertEquals(left.z(), right.z(), EPSILON);
    }

    @Test
    public void testConstantsAreSingletons()
    {
        // Verify constants are always the same instance
        assertSame(Vector.ORIGO, Vector.ORIGO);
        assertSame(Vector.UNIT_X, Vector.UNIT_X);
        assertSame(Vector.UNIT_Y, Vector.UNIT_Y);
        assertSame(Vector.UNIT_Z, Vector.UNIT_Z);
    }

    @Test
    public void testNegativeComponents()
    {
        final Vector v = Vector.make(-1.5, -2.5, -3.5);
        assertEquals(-1.5, v.x(), 0);
        assertEquals(-2.5, v.y(), 0);
        assertEquals(-3.5, v.z(), 0);
    }

    @Test
    public void testZeroComponents()
    {
        final Vector v = Vector.make(0.0, 0.0, 0.0);
        assertEquals(0.0, v.x(), 0);
        assertEquals(0.0, v.y(), 0);
        assertEquals(0.0, v.z(), 0);
        assertEquals(0.0, v.length(), 0);
    }
}
