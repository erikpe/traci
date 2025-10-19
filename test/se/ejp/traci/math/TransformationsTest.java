package se.ejp.traci.math;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static se.ejp.traci.util.AssertApprox.assertApprox;

import org.junit.Test;

public class TransformationsTest
{
    private static final double EPSILON = 1e-10;

    @Test
    public void testIdentityIsSingleton()
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
    public void testTranslateWithCoordinates()
    {
        final Transformation t = Transformations.translate(1.0, 2.0, 3.0);
        final Vector p = Vector.ORIGO;
        final Vector result = t.point(p);
        
        assertEquals(1.0, result.x(), EPSILON);
        assertEquals(2.0, result.y(), EPSILON);
        assertEquals(3.0, result.z(), EPSILON);
    }

    @Test
    public void testTranslateWithVector()
    {
        final Vector v = Vector.make(5.0, -3.0, 2.0);
        final Transformation t = Transformations.translate(v);
        final Vector p = Vector.make(1.0, 1.0, 1.0);
        final Vector result = t.point(p);
        
        assertEquals(6.0, result.x(), EPSILON);
        assertEquals(-2.0, result.y(), EPSILON);
        assertEquals(3.0, result.z(), EPSILON);
    }

    @Test
    public void testTranslateInverse()
    {
        final Transformation t = Transformations.translate(1.0, 2.0, 3.0);
        final Vector p = Vector.make(1.0, 2.0, 3.0);
        final Vector result = t.pointInv(p);
        
        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
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
    public void testScaleNonUniformCoordinates()
    {
        final Transformation t = Transformations.scale(2.0, 3.0, 4.0);
        final Vector p = Vector.make(1.0, 1.0, 1.0);
        final Vector result = t.point(p);
        
        assertEquals(2.0, result.x(), EPSILON);
        assertEquals(3.0, result.y(), EPSILON);
        assertEquals(4.0, result.z(), EPSILON);
    }

    @Test
    public void testScaleWithVector()
    {
        final Vector scaleVec = Vector.make(0.5, 2.0, 1.5);
        final Transformation t = Transformations.scale(scaleVec);
        final Vector p = Vector.make(4.0, 3.0, 2.0);
        final Vector result = t.point(p);
        
        assertEquals(2.0, result.x(), EPSILON);
        assertEquals(6.0, result.y(), EPSILON);
        assertEquals(3.0, result.z(), EPSILON);
    }

    @Test
    public void testScaleInverse()
    {
        final Transformation t = Transformations.scale(2.0, 3.0, 4.0);
        final Vector p = Vector.make(2.0, 3.0, 4.0);
        final Vector result = t.pointInv(p);
        
        assertEquals(1.0, result.x(), EPSILON);
        assertEquals(1.0, result.y(), EPSILON);
        assertEquals(1.0, result.z(), EPSILON);
    }

    @Test
    public void testScaleX()
    {
        final Transformation t = Transformations.scalex(3.0);
        final Vector p = Vector.make(2.0, 5.0, 7.0);
        final Vector result = t.point(p);
        
        assertEquals(6.0, result.x(), EPSILON);
        assertEquals(5.0, result.y(), EPSILON);
        assertEquals(7.0, result.z(), EPSILON);
    }

    @Test
    public void testScaleY()
    {
        final Transformation t = Transformations.scaley(3.0);
        final Vector p = Vector.make(2.0, 5.0, 7.0);
        final Vector result = t.point(p);
        
        assertEquals(2.0, result.x(), EPSILON);
        assertEquals(15.0, result.y(), EPSILON);
        assertEquals(7.0, result.z(), EPSILON);
    }

    @Test
    public void testScaleZ()
    {
        final Transformation t = Transformations.scalez(3.0);
        final Vector p = Vector.make(2.0, 5.0, 7.0);
        final Vector result = t.point(p);
        
        assertEquals(2.0, result.x(), EPSILON);
        assertEquals(5.0, result.y(), EPSILON);
        assertEquals(21.0, result.z(), EPSILON);
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
    public void testRotXInverse()
    {
        final Transformation t = Transformations.rotx(Math.PI / 4.0);
        final Vector p = Vector.make(1.0, 1.0, 1.0);
        
        final Vector forward = t.point(p);
        final Vector backward = t.pointInv(forward);
        
        assertEquals(p.x(), backward.x(), EPSILON);
        assertEquals(p.y(), backward.y(), EPSILON);
        assertEquals(p.z(), backward.z(), EPSILON);
    }

    @Test
    public void testRotYInverse()
    {
        final Transformation t = Transformations.roty(Math.PI / 3.0);
        final Vector p = Vector.make(2.0, 3.0, 4.0);
        
        final Vector forward = t.point(p);
        final Vector backward = t.pointInv(forward);
        
        assertEquals(p.x(), backward.x(), EPSILON);
        assertEquals(p.y(), backward.y(), EPSILON);
        assertEquals(p.z(), backward.z(), EPSILON);
    }

    @Test
    public void testRotZInverse()
    {
        final Transformation t = Transformations.rotz(Math.PI / 6.0);
        final Vector p = Vector.make(5.0, 6.0, 7.0);
        
        final Vector forward = t.point(p);
        final Vector backward = t.pointInv(forward);
        
        assertEquals(p.x(), backward.x(), EPSILON);
        assertEquals(p.y(), backward.y(), EPSILON);
        assertEquals(p.z(), backward.z(), EPSILON);
    }

    @Test
    public void testRotVecToZUnitX()
    {
        final Vector v = Vector.UNIT_X;
        final Transformation t = Transformations.rotVecToZ(v);
        final Vector result = t.point(v);
        
        // Vector should be rotated to point along positive Z axis
        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(1.0, result.z(), EPSILON);
    }

    @Test
    public void testRotVecToZUnitY()
    {
        final Vector v = Vector.UNIT_Y;
        final Transformation t = Transformations.rotVecToZ(v);
        final Vector result = t.point(v);
        
        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(1.0, result.z(), EPSILON);
    }

    @Test
    public void testRotVecToZUnitZ()
    {
        final Vector v = Vector.UNIT_Z;
        final Transformation t = Transformations.rotVecToZ(v);
        final Vector result = t.point(v);
        
        // Already along Z, should remain unchanged
        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(1.0, result.z(), EPSILON);
    }

    @Test
    public void testRotVecToZArbitrary()
    {
        final Vector v = Vector.make(1.0, 1.0, 1.0).normalize();
        final Transformation t = Transformations.rotVecToZ(v);
        final Vector result = t.point(v);
        
        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(1.0, result.z(), EPSILON);
    }

    @Test
    public void testRotZToVecUnitX()
    {
        final Vector v = Vector.UNIT_X;
        final Transformation t = Transformations.rotZToVec(v);
        final Vector zAxis = Vector.UNIT_Z;
        final Vector result = t.point(zAxis);
        
        // Z axis should be rotated to point along X
        assertEquals(1.0, result.x(), EPSILON);
        assertEquals(0.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public void testRotZToVecArbitrary()
    {
        final Vector v = Vector.make(1.0, 1.0, 1.0).normalize();
        final Transformation t = Transformations.rotZToVec(v);
        final Vector zAxis = Vector.UNIT_Z;
        final Vector result = t.point(zAxis);
        
        assertEquals(v.x(), result.x(), EPSILON);
        assertEquals(v.y(), result.y(), EPSILON);
        assertEquals(v.z(), result.z(), EPSILON);
    }

    @Test
    public void testRotVecToZAndRotZToVecAreInverses()
    {
        final Vector v = Vector.make(2.0, 3.0, 4.0).normalize();
        final Transformation toZ = Transformations.rotVecToZ(v);
        final Transformation fromZ = Transformations.rotZToVec(v);
        
        final Transformation composed = toZ.compose(fromZ);
        assertApprox(Transformations.identity(), composed, 1e-14);
    }

    @Test
    public void testRotVecToVecIdentity()
    {
        final Vector v = Vector.make(1.0, 2.0, 3.0).normalize();
        final Transformation t = Transformations.rotVecToVec(v, v);
        
        // Rotating a vector to itself should be identity
        assertApprox(Transformations.identity(), t, 1e-14);
    }

    @Test
    public void testRotVecToVecXToY()
    {
        final Transformation t = Transformations.rotVecToVec(Vector.UNIT_X, Vector.UNIT_Y);
        final Vector result = t.point(Vector.UNIT_X);
        
        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(1.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public void testRotVecToVecArbitrary()
    {
        final Vector v1 = Vector.make(1.0, 0.0, 0.0);
        final Vector v2 = Vector.make(1.0, 1.0, 0.0).normalize();
        final Transformation t = Transformations.rotVecToVec(v1, v2);
        final Vector result = t.point(v1);
        
        assertEquals(v2.x(), result.x(), EPSILON);
        assertEquals(v2.y(), result.y(), EPSILON);
        assertEquals(v2.z(), result.z(), EPSILON);
    }

    @Test
    public void testRotAroundOriginZ()
    {
        final Vector center = Vector.ORIGO;
        final Vector axis = Vector.make(0.0, 0.0, 1.0);
        final Transformation t = Transformations.rotAround(center, axis, Math.PI / 2.0);
        
        final Vector p = Vector.make(1.0, 0.0, 0.0);
        final Vector result = t.point(p);
        
        assertEquals(0.0, result.x(), EPSILON);
        assertEquals(1.0, result.y(), EPSILON);
        assertEquals(0.0, result.z(), EPSILON);
    }

    @Test
    public void testRotAroundCenterPreserved()
    {
        final Vector center = Vector.make(2.0, 3.0, 4.0);
        final Vector axis = Vector.make(2.0, 3.0, 5.0);
        final Transformation t = Transformations.rotAround(center, axis, Math.PI / 3.0);
        
        // Point at center should remain unchanged
        final Vector result = t.point(center);
        assertEquals(center.x(), result.x(), EPSILON);
        assertEquals(center.y(), result.y(), EPSILON);
        assertEquals(center.z(), result.z(), EPSILON);
    }

    @Test
    public void testRotAroundInverse()
    {
        final Vector center = Vector.make(1.0, 2.0, 3.0);
        final Vector axis = Vector.make(1.0, 2.0, 5.0);
        final double angle = Math.PI / 4.0;
        
        final Transformation t = Transformations.rotAround(center, axis, angle);
        final Transformation tInv = Transformations.rotAround(center, axis, -angle);
        
        final Vector p = Vector.make(5.0, 6.0, 7.0);
        final Vector rotated = t.point(p);
        final Vector restored = tInv.point(rotated);
        
        assertEquals(p.x(), restored.x(), 1e-14);
        assertEquals(p.y(), restored.y(), 1e-14);
        assertEquals(p.z(), restored.z(), 1e-14);
    }

    @Test
    public void testCamera()
    {
        final Vector location = Vector.make(0.0, 0.0, -10.0);
        final Vector lookAt = Vector.ORIGO;
        final Vector up = Vector.UNIT_Y;
        
        final Transformation t = Transformations.camera(location, lookAt, up);
        assertNotNull(t);
        assertNotNull(t.mat);
        assertNotNull(t.invMat);
    }

    @Test
    public void testCameraLookingAlongZ()
    {
        final Vector location = Vector.make(0.0, 0.0, -5.0);
        final Vector lookAt = Vector.ORIGO;
        final Vector up = Vector.UNIT_Y;
        
        final Transformation t = Transformations.camera(location, lookAt, up);
        
        // Camera at (0,0,-5) looking at origin should have forward along +Z
        final Vector forward = Vector.UNIT_Z;
        final Vector transformedForward = t.dir(forward);
        
        // Transformed forward should point toward lookAt
        assertNotNull(transformedForward);
        assertEquals(0.0, transformedForward.x(), EPSILON);
        assertEquals(0.0, transformedForward.y(), EPSILON);
        assertEquals(1.0, transformedForward.z(), EPSILON);
    }

    @Test
    public void testCameraAtLocation()
    {
        final Vector location = Vector.make(5.0, 10.0, 15.0);
        final Vector lookAt = Vector.make(5.0, 10.0, 16.0);
        final Vector up = Vector.UNIT_Y;
        
        final Transformation t = Transformations.camera(location, lookAt, up);
        
        // Camera transformation applied to origin should give camera location
        final Vector result = t.point(Vector.ORIGO);
        assertEquals(location.x(), result.x(), EPSILON);
        assertEquals(location.y(), result.y(), EPSILON);
        assertEquals(location.z(), result.z(), EPSILON);
    }

    @Test
    public void testMultipleTransformationsComposed()
    {
        final Transformation t1 = Transformations.translate(1.0, 0.0, 0.0);
        final Transformation t2 = Transformations.rotz(Math.PI / 2.0);
        final Transformation t3 = Transformations.scale(2.0);
        
        final Transformation composed = t1.compose(t2).compose(t3);
        final Vector p = Vector.make(1.0, 0.0, 0.0);
        final Vector result = composed.point(p);
        
        // Should work without errors
        assertNotNull(result);
    }

    @Test
    public void testTranslateCommutativity()
    {
        final Transformation t1 = Transformations.translate(1.0, 2.0, 3.0);
        final Transformation t2 = Transformations.translate(4.0, 5.0, 6.0);
        
        final Vector p = Vector.make(0.0, 0.0, 0.0);
        
        final Vector result1 = t1.compose(t2).point(p);
        final Vector result2 = t2.compose(t1).point(p);
        
        // Translations commute
        assertEquals(result1.x(), result2.x(), EPSILON);
        assertEquals(result1.y(), result2.y(), EPSILON);
        assertEquals(result1.z(), result2.z(), EPSILON);
    }

    @Test
    public void testScaleCommutativity()
    {
        final Transformation t1 = Transformations.scale(2.0, 3.0, 4.0);
        final Transformation t2 = Transformations.scale(5.0, 6.0, 7.0);
        
        final Vector p = Vector.make(1.0, 1.0, 1.0);
        
        final Vector result1 = t1.compose(t2).point(p);
        final Vector result2 = t2.compose(t1).point(p);
        
        // Scales commute
        assertEquals(result1.x(), result2.x(), EPSILON);
        assertEquals(result1.y(), result2.y(), EPSILON);
        assertEquals(result1.z(), result2.z(), EPSILON);
    }

    @Test
    public void testRotationCommutativity()
    {
        // Rotations around same axis commute
        final Transformation t1 = Transformations.rotz(Math.PI / 3.0);
        final Transformation t2 = Transformations.rotz(Math.PI / 4.0);
        
        final Vector p = Vector.make(1.0, 0.0, 0.0);
        
        final Vector result1 = t1.compose(t2).point(p);
        final Vector result2 = t2.compose(t1).point(p);
        
        assertEquals(result1.x(), result2.x(), EPSILON);
        assertEquals(result1.y(), result2.y(), EPSILON);
        assertEquals(result1.z(), result2.z(), EPSILON);
    }

    @Test
    public void testZeroTranslation()
    {
        final Transformation t = Transformations.translate(0.0, 0.0, 0.0);
        assertApprox(Transformations.identity(), t, EPSILON);
    }

    @Test
    public void testUnitScale()
    {
        final Transformation t = Transformations.scale(1.0);
        assertApprox(Transformations.identity(), t, EPSILON);
    }

    @Test
    public void testZeroRotation()
    {
        final Transformation tx = Transformations.rotx(0.0);
        final Transformation ty = Transformations.roty(0.0);
        final Transformation tz = Transformations.rotz(0.0);
        
        assertApprox(Transformations.identity(), tx, EPSILON);
        assertApprox(Transformations.identity(), ty, EPSILON);
        assertApprox(Transformations.identity(), tz, EPSILON);
    }
}
