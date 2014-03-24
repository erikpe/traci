package se.ejp.traci.render;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import se.ejp.traci.render.Point.Type;

public class RayTest extends RayBase
{
    @Test
    public void testCheckRay()
    {
        Ray ray = null;
        assertTrue(Ray.checkRay(null));

        ray = makeRay(Point.make(1.0, p0, Type.INTERSECT, null));
        assertTrue(Ray.checkRay(ray));

        ray = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null));
        assertTrue(Ray.checkRay(ray));

        ray = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null),
                Point.make(3.0, p1, Type.INTERSECT, null));
        assertTrue(Ray.checkRay(ray));

        ray = makeRay(
                Point.make(0.5, p0, Type.INTERSECT, null),
                Point.make(1.0, p1, Type.ENTER, null),
                Point.make(2.0, p1, Type.LEAVE, null));
        assertTrue(Ray.checkRay(ray));
    }

    @Test
    public void testCheckRayInvalid()
    {
        Ray ray = Ray.make();
        assertFalse(Ray.checkRay(ray));

        ray = makeRay(
                Point.make(2.0, p0, Type.ENTER, null),
                Point.make(1.0, p0, Type.LEAVE, null));
        assertFalse(Ray.checkRay(ray));

        ray = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(1.5, p1, Type.INTERSECT, null),
                Point.make(2.0, p0, Type.LEAVE, null));
        assertFalse(Ray.checkRay(ray));

        ray = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(1.5, p1, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null),
                Point.make(2.5, p1, Type.LEAVE, null));
        assertFalse(Ray.checkRay(ray));

        ray = makeRay(
                Point.make(1.0, p0, Type.INTERSECT, null),
                Point.make(1.5, p1, Type.ENTER, null),
                Point.make(2.0, p2, Type.INTERSECT, null));
        assertFalse(Ray.checkRay(ray));

        ray = makeRay(
                Point.make(1.0, p0, Type.INTERSECT, null),
                Point.make(1.5, p1, Type.LEAVE, null),
                Point.make(2.0, p2, Type.INTERSECT, null));
        assertFalse(Ray.checkRay(ray));

        ray = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(1.5, p1, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null));
        assertFalse(Ray.checkRay(ray));

        ray = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(1.5, p0, Type.LEAVE, null),
                Point.make(2.0, p0, Type.LEAVE, null));
        assertFalse(Ray.checkRay(ray));
    }
}
