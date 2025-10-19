package se.ejp.traci.math;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import static se.ejp.traci.util.AssertApprox.assertApprox;

import org.junit.Test;

public class MatrixTest
{
    private static final double EPSILON = 1e-10;

    @Test
    public final void testEye()
    {
        final Matrix eye = Matrix.eye();
        for (int row = 0; row < 4; ++row)
        {
            for (int col = 0; col < 4; ++col)
            {
                final double expected = (row == col) ? 1.0 : 0.0;
                assertEquals(expected, eye.at(row, col), 0.0);
            }
        }
    }

    @Test
    public final void testMatrix()
    {
        final Vector v0 = Vector.make(1.0, 2.3, 3.0);
        final Vector v1 = Vector.make(-v0.x(), -v0.y(), -v0.z());
        final Vector v2 = Vector.make(1.0 / v0.x(), 1.0 / v0.y(), 1.0 / v0.z());

        assertApprox(Matrix.eye(), Matrix.eye().mul(Matrix.eye()));
        assertApprox(Matrix.eye(), Matrix.rotx(2.23).mul(Matrix.rotx(-2.23)));
        assertApprox(Matrix.eye(), Matrix.roty(3.23).mul(Matrix.roty(-3.23)));
        assertApprox(Matrix.eye(), Matrix.rotz(4.23).mul(Matrix.rotz(-4.23)));
        assertApprox(Matrix.eye(), Matrix.scale(v0).mul(Matrix.scale(v2)));
        assertApprox(Matrix.eye(), Matrix.translate(v0).mul(Matrix.translate(v1)));
    }

    @Test
    public final void testRotxBasic()
    {
        final Matrix rotx90 = Matrix.rotx(Math.PI / 2.0);
        final Vector v = Vector.make(1.0, 0.0, 0.0);
        final Vector result = rotx90.mul(v);

        assertEquals(1.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public final void testRotxYAxisRotation()
    {
        final Matrix rotx90 = Matrix.rotx(Math.PI / 2.0);
        final Vector v = Vector.make(0.0, 1.0, 0.0);
        final Vector result = rotx90.mul(v);

        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(1.0, result.z(), EPSILON);
    }

    @Test
    public final void testRotxZAxisRotation()
    {
        final Matrix rotx90 = Matrix.rotx(Math.PI / 2.0);
        final Vector v = Vector.make(0.0, 0.0, 1.0);
        final Vector result = rotx90.mul(v);

        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(-1.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public final void testRotyBasic()
    {
        final Matrix roty90 = Matrix.roty(Math.PI / 2.0);
        final Vector v = Vector.make(0.0, 1.0, 0.0);
        final Vector result = roty90.mul(v);

        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(1.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public final void testRotyXAxisRotation()
    {
        final Matrix roty90 = Matrix.roty(Math.PI / 2.0);
        final Vector v = Vector.make(1.0, 0.0, 0.0);
        final Vector result = roty90.mul(v);

        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(-1.0, result.z(), EPSILON);
    }

    @Test
    public final void testRotyZAxisRotation()
    {
        final Matrix roty90 = Matrix.roty(Math.PI / 2.0);
        final Vector v = Vector.make(0.0, 0.0, 1.0);
        final Vector result = roty90.mul(v);

        assertEquals(1.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public final void testRotzBasic()
    {
        final Matrix rotz90 = Matrix.rotz(Math.PI / 2.0);
        final Vector v = Vector.make(0.0, 0.0, 1.0);
        final Vector result = rotz90.mul(v);

        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(1.0, result.z(), EPSILON);
    }

    @Test
    public final void testRotzXAxisRotation()
    {
        final Matrix rotz90 = Matrix.rotz(Math.PI / 2.0);
        final Vector v = Vector.make(1.0, 0.0, 0.0);
        final Vector result = rotz90.mul(v);

        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(1.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public final void testRotzYAxisRotation()
    {
        final Matrix rotz90 = Matrix.rotz(Math.PI / 2.0);
        final Vector v = Vector.make(0.0, 1.0, 0.0);
        final Vector result = rotz90.mul(v);

        assertEquals(-1.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public final void testScale()
    {
        final Vector scaleVec = Vector.make(2.0, 3.0, 4.0);
        final Matrix scaleMat = Matrix.scale(scaleVec);
        final Vector v = Vector.make(1.0, 1.0, 1.0);
        final Vector result = scaleMat.mul(v);

        assertEquals(2.0, result.x(), EPSILON);
        assertEquals(3.0, result.y(), EPSILON);
        assertEquals(4.0, result.z(), EPSILON);
    }

    @Test
    public final void testScaleNonUniform()
    {
        final Vector scaleVec = Vector.make(0.5, 2.0, 1.0);
        final Matrix scaleMat = Matrix.scale(scaleVec);
        final Vector v = Vector.make(4.0, 2.0, 8.0);
        final Vector result = scaleMat.mul(v);

        assertEquals(2.0, result.x(), EPSILON);
        assertEquals(4.0, result.y(), EPSILON);
        assertEquals(8.0, result.z(), EPSILON);
    }

    @Test
    public final void testTranslate()
    {
        final Vector translateVec = Vector.make(1.0, 2.0, 3.0);
        final Matrix translateMat = Matrix.translate(translateVec);
        final Vector v = Vector.make(0.0, 0.0, 0.0);
        final Vector result = translateMat.mul(v);

        assertEquals(1.0, result.x(), EPSILON);
        assertEquals(2.0, result.y(), EPSILON);
        assertEquals(3.0, result.z(), EPSILON);
    }

    @Test
    public final void testTranslateNonZeroPoint()
    {
        final Vector translateVec = Vector.make(5.0, -3.0, 2.0);
        final Matrix translateMat = Matrix.translate(translateVec);
        final Vector v = Vector.make(1.0, 1.0, 1.0);
        final Vector result = translateMat.mul(v);

        assertEquals(6.0, result.x(), EPSILON);
        assertEquals(-2.0, result.y(), EPSILON);
        assertEquals(3.0, result.z(), EPSILON);
    }

    @Test
    public final void testMake()
    {
        final Vector v0 = Vector.make(1.0, 0.0, 0.0);
        final Vector v1 = Vector.make(0.0, 1.0, 0.0);
        final Vector v2 = Vector.make(0.0, 0.0, 1.0);
        final Matrix mat = Matrix.make(v0, v1, v2);

        assertEquals(1.0, mat.at(0, 0), EPSILON);
        assertEquals(0.0, mat.at(0, 1), EPSILON);
        assertEquals(0.0, mat.at(0, 2), EPSILON);
        assertEquals(0.0, mat.at(1, 0), EPSILON);
        assertEquals(1.0, mat.at(1, 1), EPSILON);
        assertEquals(0.0, mat.at(1, 2), EPSILON);
        assertEquals(0.0, mat.at(2, 0), EPSILON);
        assertEquals(0.0, mat.at(2, 1), EPSILON);
        assertEquals(1.0, mat.at(2, 2), EPSILON);
    }

    @Test
    public final void testMakeCustomVectors()
    {
        final Vector v0 = Vector.make(1.0, 2.0, 3.0);
        final Vector v1 = Vector.make(4.0, 5.0, 6.0);
        final Vector v2 = Vector.make(7.0, 8.0, 9.0);
        final Matrix mat = Matrix.make(v0, v1, v2);

        assertEquals(1.0, mat.at(0, 0), EPSILON);
        assertEquals(4.0, mat.at(0, 1), EPSILON);
        assertEquals(7.0, mat.at(0, 2), EPSILON);
        assertEquals(2.0, mat.at(1, 0), EPSILON);
        assertEquals(5.0, mat.at(1, 1), EPSILON);
        assertEquals(8.0, mat.at(1, 2), EPSILON);
        assertEquals(3.0, mat.at(2, 0), EPSILON);
        assertEquals(6.0, mat.at(2, 1), EPSILON);
        assertEquals(9.0, mat.at(2, 2), EPSILON);
    }

    @Test
    public final void testMulMatrixAssociativity()
    {
        final Matrix m1 = Matrix.rotx(0.5);
        final Matrix m2 = Matrix.roty(0.7);
        final Matrix m3 = Matrix.rotz(0.9);

        final Matrix result1 = m1.mul(m2).mul(m3);
        final Matrix result2 = m1.mul(m2.mul(m3));

        // Matrix multiplication can accumulate rounding errors, use a small epsilon
        assertApprox(result1, result2, 1e-15);
    }

    @Test
    public final void testMulVectorWithTranslation()
    {
        final Matrix translate = Matrix.translate(Vector.make(1.0, 2.0, 3.0));
        final Matrix scale = Matrix.scale(Vector.make(2.0, 2.0, 2.0));
        final Matrix combined = translate.mul(scale);

        final Vector v = Vector.make(1.0, 1.0, 1.0);
        final Vector result = combined.mul(v);

        assertEquals(3.0, result.x(), EPSILON);
        assertEquals(4.0, result.y(), EPSILON);
        assertEquals(5.0, result.z(), EPSILON);
    }

    @Test
    public final void testMulDirIgnoresTranslation()
    {
        final Vector translateVec = Vector.make(10.0, 20.0, 30.0);
        final Matrix translateMat = Matrix.translate(translateVec);
        final Vector dir = Vector.make(1.0, 0.0, 0.0);
        final Vector result = translateMat.mulDir(dir);

        assertEquals(1.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public final void testMulDirWithRotation()
    {
        final Matrix rotz90 = Matrix.rotz(Math.PI / 2.0);
        final Vector dir = Vector.make(1.0, 0.0, 0.0);
        final Vector result = rotz90.mulDir(dir);

        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(1.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public final void testMulDirWithScale()
    {
        final Matrix scale = Matrix.scale(Vector.make(2.0, 3.0, 4.0));
        final Vector dir = Vector.make(1.0, 1.0, 1.0);
        final Vector result = scale.mulDir(dir);

        assertEquals(2.0, result.x(), EPSILON);
        assertEquals(3.0, result.y(), EPSILON);
        assertEquals(4.0, result.z(), EPSILON);
    }

    @Test
    public final void testMulNormalTranspose()
    {
        final Vector v0 = Vector.make(1.0, 0.0, 0.0);
        final Vector v1 = Vector.make(0.0, 1.0, 0.0);
        final Vector v2 = Vector.make(0.0, 0.0, 1.0);
        final Matrix mat = Matrix.make(v0, v1, v2);

        final Vector normal = Vector.make(1.0, 2.0, 3.0);
        final Vector result = mat.mulNormal(normal);

        assertEquals(1.0, result.x(), EPSILON);
        assertEquals(2.0, result.y(), EPSILON);
        assertEquals(3.0, result.z(), EPSILON);
    }

    @Test
    public final void testMulNormalWithCustomMatrix()
    {
        final Vector v0 = Vector.make(2.0, 0.0, 0.0);
        final Vector v1 = Vector.make(0.0, 3.0, 0.0);
        final Vector v2 = Vector.make(0.0, 0.0, 4.0);
        final Matrix mat = Matrix.make(v0, v1, v2);

        final Vector normal = Vector.make(1.0, 1.0, 1.0);
        final Vector result = mat.mulNormal(normal);

        assertEquals(2.0, result.x(), EPSILON);
        assertEquals(3.0, result.y(), EPSILON);
        assertEquals(4.0, result.z(), EPSILON);
    }

    @Test
    public final void testEqualsReflexive()
    {
        final Matrix m = Matrix.eye();
        assertTrue(m.equals(m));
    }

    @Test
    public final void testEqualsNull()
    {
        final Matrix m = Matrix.eye();
        assertFalse(m.equals(null));
    }

    @Test
    public final void testEqualsDifferentClass()
    {
        final Matrix m = Matrix.eye();
        assertFalse(m.equals("not a matrix"));
    }

    @Test
    public final void testEqualsIdenticalMatrices()
    {
        final Matrix m1 = Matrix.rotx(0.5);
        final Matrix m2 = Matrix.rotx(0.5);
        assertTrue(m1.equals(m2));
    }

    @Test
    public final void testEqualsDifferentMatrices()
    {
        final Matrix m1 = Matrix.rotx(0.5);
        final Matrix m2 = Matrix.rotx(0.6);
        assertFalse(m1.equals(m2));
    }

    @Test
    public final void testHashCodeConsistency()
    {
        final Matrix m = Matrix.rotx(0.5);
        final int hash1 = m.hashCode();
        final int hash2 = m.hashCode();
        assertEquals(hash1, hash2);
    }

    @Test
    public final void testHashCodeEqualMatrices()
    {
        final Matrix m1 = Matrix.rotx(0.5);
        final Matrix m2 = Matrix.rotx(0.5);
        assertEquals(m1.hashCode(), m2.hashCode());
    }

    @Test
    public final void testHashCodeDifferentMatrices()
    {
        final Matrix m1 = Matrix.rotx(0.5);
        final Matrix m2 = Matrix.roty(0.5);
        assertNotEquals(m1.hashCode(), m2.hashCode());
    }

    @Test
    public final void testToString()
    {
        final Matrix m = Matrix.eye();
        final String str = m.toString();
        assertTrue(str.contains("["));
        assertTrue(str.contains("]"));
        assertTrue(str.contains("1.0"));
        assertTrue(str.contains("0.0"));
    }

    @Test
    public final void testAtAccessAllElements()
    {
        final Vector v0 = Vector.make(1.0, 2.0, 3.0);
        final Vector v1 = Vector.make(4.0, 5.0, 6.0);
        final Vector v2 = Vector.make(7.0, 8.0, 9.0);
        final Matrix mat = Matrix.make(v0, v1, v2);

        // Test accessing all elements via at() method
        for (int row = 0; row < 4; ++row)
        {
            for (int col = 0; col < 4; ++col)
            {
                final double value = mat.at(row, col);
                // Just verify we can access without exception
                assertTrue(Double.isFinite(value));
            }
        }
    }

    @Test
    public final void testComposedTransformations()
    {
        // Test a realistic composition: translate then rotate then scale
        final Matrix translate = Matrix.translate(Vector.make(1.0, 0.0, 0.0));
        final Matrix rotate = Matrix.rotz(Math.PI / 4.0);
        final Matrix scale = Matrix.scale(Vector.make(2.0, 2.0, 2.0));

        final Matrix composed = scale.mul(rotate).mul(translate);
        final Vector v = Vector.make(0.0, 0.0, 0.0);
        final Vector result = composed.mul(v);

        // After translate: (1, 0, 0)
        // After rotate 45°: (cos(45°), sin(45°), 0) ≈ (0.707, 0.707, 0)
        // After scale by 2: (1.414, 1.414, 0)
        assertEquals(Math.sqrt(2.0), result.x(), EPSILON);
        assertEquals(Math.sqrt(2.0), result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public final void testRotationAngles()
    {
        // Test with specific sin/cos values
        final double sin45 = Math.sin(Math.PI / 4.0);
        final double cos45 = Math.cos(Math.PI / 4.0);

        final Matrix rotx = Matrix.rotx(sin45, cos45);
        final Matrix roty = Matrix.roty(sin45, cos45);
        final Matrix rotz = Matrix.rotz(sin45, cos45);

        // Verify they produce same results as angle-based constructors
        final Matrix rotxAngle = Matrix.rotx(Math.PI / 4.0);
        final Matrix rotyAngle = Matrix.roty(Math.PI / 4.0);
        final Matrix rotzAngle = Matrix.rotz(Math.PI / 4.0);

        assertApprox(rotx, rotxAngle);
        assertApprox(roty, rotyAngle);
        assertApprox(rotz, rotzAngle);
    }

    @Test
    public final void testZeroRotation()
    {
        final Matrix rotx0 = Matrix.rotx(0.0);
        final Matrix roty0 = Matrix.roty(0.0);
        final Matrix rotz0 = Matrix.rotz(0.0);

        assertApprox(Matrix.eye(), rotx0);
        assertApprox(Matrix.eye(), roty0);
        assertApprox(Matrix.eye(), rotz0);
    }

    @Test
    public final void testFullRotation()
    {
        final double twoPi = 2.0 * Math.PI;

        final Matrix rotx360 = Matrix.rotx(twoPi);
        final Matrix roty360 = Matrix.roty(twoPi);
        final Matrix rotz360 = Matrix.rotz(twoPi);

        // Full rotation should be close to identity, but floating-point error accumulates
        assertApprox(Matrix.eye(), rotx360, 1e-15);
        assertApprox(Matrix.eye(), roty360, 1e-15);
        assertApprox(Matrix.eye(), rotz360, 1e-15);
    }
}
