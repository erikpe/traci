package se.ejp.traci.math;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static se.ejp.traci.util.AssertApprox.assertApprox;
import static se.ejp.traci.util.AssertNotApprox.assertNotApprox;

import org.junit.Test;

public class TransformationTest
{
    private static final double EPSILON = 1e-10;

    @Test
    public void testIdentity()
    {
        final Transformation t1 = Transformations.translate(1.0, 2.5, 3.0);
        final Transformation t2 = Transformations.identity();

        final Transformation t3 = t1.compose(t2);
        final Transformation t4 = t2.compose(t1);

        assertApprox(t1, t3);
        assertApprox(t1, t4);
        assertApprox(t3, t4);
    }

    @Test
    public void testAssociativity()
    {
        final Transformation t1 = Transformations.translate(1.0, 2.5, 3.0);
        final Transformation t2 = Transformations.rotx(2.23);
        final Transformation t3 = Transformations.scaley(.45);

        final Transformation t4 = t1.compose(t2).compose(t3);
        final Transformation t5 = t1.compose(t2.compose(t3));

        assertApprox(t4, t5);
    }

    @Test
    public void testNonCommutativity()
    {
        final Transformation t1 = Transformations.translate(1.0, 2.5, 3.0);
        final Transformation t2 = Transformations.rotx(2.23);

        final Transformation t3 = t1.compose(t2);
        final Transformation t4 = t2.compose(t1);

        assertNotApprox(t3, t4);
    }

    @Test
    public void testInvert()
    {
        final Transformation eye = Transformations.identity();
        final Vector v0 = Vector.make(1.0, 2.3, 3.0);
        final Vector v1 = Vector.make(-v0.x(), -v0.y(), -v0.z());
        final Vector v2 = Vector.make(1.0 / v0.x(), 1.0 / v0.y(), 1.0 / v0.z());
        final Vector v3 = Vector.make(1.5, 4.4, 5.0);

        assertApprox(eye, Transformations.translate(v0).compose(Transformations.translate(v1)));
        assertApprox(eye, Transformations.scale(v0).compose(Transformations.scale(v2)));
        assertApprox(eye, Transformations.scalex(2.23).compose(Transformations.scalex(1.0 / 2.23)));
        assertApprox(eye, Transformations.scaley(3.23).compose(Transformations.scaley(1.0 / 3.23)));
        assertApprox(eye, Transformations.scalez(4.23).compose(Transformations.scalez(1.0 / 4.23)));
        assertApprox(eye, Transformations.rotx(2.23).compose(Transformations.rotx(-2.23)));
        assertApprox(eye, Transformations.roty(3.23).compose(Transformations.roty(-3.23)));
        assertApprox(eye, Transformations.rotz(4.23).compose(Transformations.rotz(-4.23)));
        assertApprox(eye, Transformations.rotVecToZ(v0).compose(Transformations.rotZToVec(v0)), 1e-14);
        assertApprox(eye, Transformations.rotZToVec(v0).compose(Transformations.rotVecToZ(v0)), 1e-14);
        assertApprox(eye, Transformations.rotVecToVec(v0, v3).compose(Transformations.rotVecToVec(v3, v0)), 1e-14);
        assertApprox(eye, Transformations.rotAround(v0, v3, 2.23).compose(Transformations.rotAround(v0, v3, -2.23)), 1e-14);
        assertApprox(eye, Transformations.rotAround(v0, v3, 2.23).compose(Transformations.rotAround(v3, v0, 2.23)), 1e-14);
    }

    // New comprehensive tests below

    @Test
    public void testIdentityIsConstant()
    {
        final Transformation id1 = Transformations.identity();
        final Transformation id2 = Transformations.identity();
        assertSame(id1, id2);
    }

    @Test
    public void testIdentityMatrices()
    {
        final Transformation identity = Transformations.identity();
        assertEquals(Matrix.eye(), identity.mat);
        assertEquals(Matrix.eye(), identity.invMat);
    }

    @Test
    public void testInvertMethod()
    {
        final Transformation t = Transformations.translate(1.0, 2.0, 3.0);
        final Transformation inverted = t.invert();
        
        // Inverted transformation should have swapped matrices
        assertEquals(t.mat, inverted.invMat);
        assertEquals(t.invMat, inverted.mat);
    }

    @Test
    public void testInvertDoubleInvert()
    {
        final Transformation t = Transformations.rotx(0.5);
        final Transformation doubleInverted = t.invert().invert();
        
        assertApprox(t, doubleInverted);
    }

    @Test
    public void testInvertWithCompose()
    {
        final Transformation t = Transformations.translate(1.0, 2.0, 3.0);
        final Transformation composed = t.compose(t.invert());
        
        assertApprox(Transformations.identity(), composed, 1e-15);
    }

    @Test
    public void testPointTranslation()
    {
        final Transformation t = Transformations.translate(1.0, 2.0, 3.0);
        final Vector p = Vector.make(0.0, 0.0, 0.0);
        final Vector result = t.point(p);
        
        assertEquals(1.0, result.x(), EPSILON);
        assertEquals(2.0, result.y(), EPSILON);
        assertEquals(3.0, result.z(), EPSILON);
    }

    @Test
    public void testPointScale()
    {
        final Transformation t = Transformations.scale(2.0, 3.0, 4.0);
        final Vector p = Vector.make(1.0, 1.0, 1.0);
        final Vector result = t.point(p);
        
        assertEquals(2.0, result.x(), EPSILON);
        assertEquals(3.0, result.y(), EPSILON);
        assertEquals(4.0, result.z(), EPSILON);
    }

    @Test
    public void testPointRotation()
    {
        final Transformation t = Transformations.rotz(Math.PI / 2.0);
        final Vector p = Vector.make(1.0, 0.0, 0.0);
        final Vector result = t.point(p);
        
        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(1.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public void testPointInv()
    {
        final Transformation t = Transformations.translate(1.0, 2.0, 3.0);
        final Vector p = Vector.make(1.0, 2.0, 3.0);
        final Vector result = t.pointInv(p);
        
        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public void testPointAndPointInvAreInverses()
    {
        final Transformation t = Transformations.rotx(0.7);
        final Vector p = Vector.make(1.5, 2.5, 3.5);
        
        final Vector transformed = t.point(p);
        final Vector restored = t.pointInv(transformed);
        
        assertEquals(p.x(), restored.x(), EPSILON);
        assertEquals(p.y(), restored.y(), EPSILON);
        assertEquals(p.z(), restored.z(), EPSILON);
    }

    @Test
    public void testDirIgnoresTranslation()
    {
        final Transformation t = Transformations.translate(10.0, 20.0, 30.0);
        final Vector dir = Vector.make(1.0, 0.0, 0.0);
        final Vector result = t.dir(dir);
        
        // Direction should be unchanged by translation
        assertEquals(1.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public void testDirWithRotation()
    {
        final Transformation t = Transformations.rotz(Math.PI / 2.0);
        final Vector dir = Vector.make(1.0, 0.0, 0.0);
        final Vector result = t.dir(dir);
        
        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(1.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public void testDirWithScale()
    {
        final Transformation t = Transformations.scale(2.0, 3.0, 4.0);
        final Vector dir = Vector.make(1.0, 1.0, 1.0);
        final Vector result = t.dir(dir);
        
        assertEquals(2.0, result.x(), EPSILON);
        assertEquals(3.0, result.y(), EPSILON);
        assertEquals(4.0, result.z(), EPSILON);
    }

    @Test
    public void testDirInv()
    {
        final Transformation t = Transformations.scale(2.0, 2.0, 2.0);
        final Vector dir = Vector.make(2.0, 2.0, 2.0);
        final Vector result = t.dirInv(dir);
        
        assertEquals(1.0, result.x(), EPSILON);
        assertEquals(1.0, result.y(), EPSILON);
        assertEquals(1.0, result.z(), EPSILON);
    }

    @Test
    public void testDirAndDirInvAreInverses()
    {
        final Transformation t = Transformations.roty(1.2);
        final Vector dir = Vector.make(1.0, 2.0, 3.0);
        
        final Vector transformed = t.dir(dir);
        final Vector restored = t.dirInv(transformed);
        
        assertEquals(dir.x(), restored.x(), EPSILON);
        assertEquals(dir.y(), restored.y(), EPSILON);
        assertEquals(dir.z(), restored.z(), EPSILON);
    }

    @Test
    public void testNormal()
    {
        final Transformation t = Transformations.scale(2.0, 1.0, 1.0);
        final Vector normal = Vector.make(1.0, 0.0, 0.0);
        final Vector result = t.normal(normal);
        
        // Normals transform differently (by inverse transpose)
        assertEquals(0.5, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public void testNormalInv()
    {
        final Transformation t = Transformations.scale(2.0, 1.0, 1.0);
        final Vector normal = Vector.make(1.0, 0.0, 0.0);
        final Vector result = t.normalInv(normal);
        
        assertEquals(2.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public void testNormalAndNormalInvAreInverses()
    {
        final Transformation t = Transformations.scale(2.0, 3.0, 4.0);
        final Vector normal = Vector.make(1.0, 1.0, 1.0);
        
        final Vector transformed = t.normal(normal);
        final Vector restored = t.normalInv(transformed);
        
        assertEquals(normal.x(), restored.x(), EPSILON);
        assertEquals(normal.y(), restored.y(), EPSILON);
        assertEquals(normal.z(), restored.z(), EPSILON);
    }

    @Test
    public void testComposeOrder()
    {
        // compose(t) computes: newMat = t.mat * this.mat
        // So translate.compose(scale) = scale.mat * translate.mat
        // This means translate is applied first, then scale
        final Transformation translate = Transformations.translate(1.0, 0.0, 0.0);
        final Transformation scale = Transformations.scale(2.0);
        
        final Vector p = Vector.make(1.0, 0.0, 0.0);
        
        final Transformation composed = translate.compose(scale);
        final Vector result = composed.point(p);
        
        // First translate: (1, 0, 0) + (1, 0, 0) = (2, 0, 0)
        // Then scale: (2, 0, 0) * 2 = (4, 0, 0)
        assertEquals(4.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public void testEqualsReflexive()
    {
        final Transformation t = Transformations.rotx(0.5);
        assertTrue(t.equals(t));
    }

    @Test
    public void testEqualsNull()
    {
        final Transformation t = Transformations.rotx(0.5);
        assertFalse(t.equals(null));
    }

    @Test
    public void testEqualsDifferentClass()
    {
        final Transformation t = Transformations.rotx(0.5);
        assertFalse(t.equals("not a transformation"));
    }

    @Test
    public void testEqualsIdentical()
    {
        final Transformation t1 = Transformations.rotx(0.5);
        final Transformation t2 = Transformations.rotx(0.5);
        assertTrue(t1.equals(t2));
    }

    @Test
    public void testEqualsDifferent()
    {
        final Transformation t1 = Transformations.rotx(0.5);
        final Transformation t2 = Transformations.rotx(0.6);
        assertFalse(t1.equals(t2));
    }

    @Test
    public void testEqualsDifferentInverse()
    {
        final Transformation t1 = Transformations.scale(2.0);
        final Transformation t2 = Transformation.make(t1.mat, Matrix.eye());
        assertFalse(t1.equals(t2));
    }

    @Test
    public void testHashCodeConsistency()
    {
        final Transformation t = Transformations.rotx(0.5);
        final int hash1 = t.hashCode();
        final int hash2 = t.hashCode();
        assertEquals(hash1, hash2);
    }

    @Test
    public void testHashCodeEqualTransformations()
    {
        final Transformation t1 = Transformations.rotx(0.5);
        final Transformation t2 = Transformations.rotx(0.5);
        assertEquals(t1.hashCode(), t2.hashCode());
    }

    @Test
    public void testHashCodeDifferentTransformations()
    {
        final Transformation t1 = Transformations.rotx(0.5);
        final Transformation t2 = Transformations.roty(0.5);
        assertNotEquals(t1.hashCode(), t2.hashCode());
    }

    @Test
    public void testToString()
    {
        final Transformation t = Transformations.identity();
        final String str = t.toString();
        assertNotNull(str);
        assertTrue(str.contains("Transformation"));
    }

    @Test
    public void testTranslateWithVector()
    {
        final Vector v = Vector.make(1.0, 2.0, 3.0);
        final Transformation t = Transformations.translate(v);
        final Vector p = Vector.ORIGO;
        final Vector result = t.point(p);
        
        assertEquals(1.0, result.x(), EPSILON);
        assertEquals(2.0, result.y(), EPSILON);
        assertEquals(3.0, result.z(), EPSILON);
    }

    @Test
    public void testScaleUniform()
    {
        final Transformation t = Transformations.scale(2.0);
        final Vector p = Vector.make(1.0, 2.0, 3.0);
        final Vector result = t.point(p);
        
        assertEquals(2.0, result.x(), EPSILON);
        assertEquals(4.0, result.y(), EPSILON);
        assertEquals(6.0, result.z(), EPSILON);
    }

    @Test
    public void testScaleNonUniform()
    {
        final Transformation t = Transformations.scale(2.0, 3.0, 4.0);
        final Vector p = Vector.make(1.0, 1.0, 1.0);
        final Vector result = t.point(p);
        
        assertEquals(2.0, result.x(), EPSILON);
        assertEquals(3.0, result.y(), EPSILON);
        assertEquals(4.0, result.z(), EPSILON);
    }

    @Test
    public void testScaleX()
    {
        final Transformation t = Transformations.scalex(2.0);
        final Vector p = Vector.make(1.0, 2.0, 3.0);
        final Vector result = t.point(p);
        
        assertEquals(2.0, result.x(), EPSILON);
        assertEquals(2.0, result.y(), EPSILON);
        assertEquals(3.0, result.z(), EPSILON);
    }

    @Test
    public void testScaleY()
    {
        final Transformation t = Transformations.scaley(2.0);
        final Vector p = Vector.make(1.0, 2.0, 3.0);
        final Vector result = t.point(p);
        
        assertEquals(1.0, result.x(), EPSILON);
        assertEquals(4.0, result.y(), EPSILON);
        assertEquals(3.0, result.z(), EPSILON);
    }

    @Test
    public void testScaleZ()
    {
        final Transformation t = Transformations.scalez(2.0);
        final Vector p = Vector.make(1.0, 2.0, 3.0);
        final Vector result = t.point(p);
        
        assertEquals(1.0, result.x(), EPSILON);
        assertEquals(2.0, result.y(), EPSILON);
        assertEquals(6.0, result.z(), EPSILON);
    }

    @Test
    public void testRotX90Degrees()
    {
        final Transformation t = Transformations.rotx(Math.PI / 2.0);
        final Vector p = Vector.make(0.0, 1.0, 0.0);
        final Vector result = t.point(p);
        
        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(1.0, result.z(), EPSILON);
    }

    @Test
    public void testRotY90Degrees()
    {
        final Transformation t = Transformations.roty(Math.PI / 2.0);
        final Vector p = Vector.make(1.0, 0.0, 0.0);
        final Vector result = t.point(p);
        
        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(-1.0, result.z(), EPSILON);
    }

    @Test
    public void testRotZ90Degrees()
    {
        final Transformation t = Transformations.rotz(Math.PI / 2.0);
        final Vector p = Vector.make(1.0, 0.0, 0.0);
        final Vector result = t.point(p);
        
        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(1.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public void testRotVecToZ()
    {
        final Vector v = Vector.make(1.0, 1.0, 1.0).normalize();
        final Transformation t = Transformations.rotVecToZ(v);
        final Vector result = t.point(v);
        
        // Should rotate v to point along Z axis
        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(v.length(), result.z(), EPSILON);
    }

    @Test
    public void testRotZToVec()
    {
        final Vector v = Vector.make(1.0, 1.0, 1.0).normalize();
        final Transformation t = Transformations.rotZToVec(v);
        final Vector zAxis = Vector.make(0.0, 0.0, 1.0);
        final Vector result = t.point(zAxis);
        
        // Should rotate Z axis to point along v
        assertEquals(v.x(), result.x(), EPSILON);
        assertEquals(v.y(), result.y(), EPSILON);
        assertEquals(v.z(), result.z(), EPSILON);
    }

    @Test
    public void testRotVecToVec()
    {
        final Vector v1 = Vector.make(1.0, 0.0, 0.0);
        final Vector v2 = Vector.make(0.0, 1.0, 0.0);
        final Transformation t = Transformations.rotVecToVec(v1, v2);
        final Vector result = t.point(v1);
        
        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(1.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public void testRotAround()
    {
        final Vector center = Vector.make(1.0, 0.0, 0.0);
        final Vector axis = Vector.make(1.0, 0.0, 1.0);
        final Transformation t = Transformations.rotAround(center, axis, Math.PI / 2.0);
        
        // Point at center should remain unchanged
        final Vector result = t.point(center);
        assertEquals(center.x(), result.x(), EPSILON);
        assertEquals(center.y(), result.y(), EPSILON);
        assertEquals(center.z(), result.z(), EPSILON);
    }

    @Test
    public void testCamera()
    {
        final Vector location = Vector.make(0.0, 0.0, -5.0);
        final Vector lookAt = Vector.ORIGO;
        final Vector up = Vector.UNIT_Y;
        
        final Transformation t = Transformations.camera(location, lookAt, up);
        assertNotNull(t);
        assertNotNull(t.mat);
        assertNotNull(t.invMat);
    }

    @Test
    public void testComplexComposition()
    {
        // Test a realistic transformation chain
        final Transformation translate1 = Transformations.translate(1.0, 0.0, 0.0);
        final Transformation rotate = Transformations.rotz(Math.PI / 4.0);
        final Transformation scale = Transformations.scale(2.0);
        final Transformation translate2 = Transformations.translate(-1.0, 0.0, 0.0);
        
        final Transformation composed = translate1.compose(rotate).compose(scale).compose(translate2);
        
        final Vector p = Vector.make(1.0, 0.0, 0.0);
        final Vector result = composed.point(p);
        
        assertNotNull(result);
        assertTrue(Double.isFinite(result.x()));
        assertTrue(Double.isFinite(result.y()));
        assertTrue(Double.isFinite(result.z()));
    }

    @Test
    public void testInverseMatricesAreCorrect()
    {
        final Transformation t = Transformations.translate(1.0, 2.0, 3.0);
        
        // Forward then inverse should give identity
        final Vector p = Vector.make(5.0, 6.0, 7.0);
        final Vector forward = t.point(p);
        final Vector backward = t.pointInv(forward);
        
        assertEquals(p.x(), backward.x(), EPSILON);
        assertEquals(p.y(), backward.y(), EPSILON);
        assertEquals(p.z(), backward.z(), EPSILON);
    }
}
