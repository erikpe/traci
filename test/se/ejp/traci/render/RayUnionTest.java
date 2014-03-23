package se.ejp.traci.render;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

import se.ejp.traci.model.shape.primitive.Primitive;
import se.ejp.traci.model.shape.primitive.Sphere;
import se.ejp.traci.render.Point.Type;

public class RayUnionTest
{
    Primitive p0 = null;
    Primitive p1 = null;
    Primitive p2 = null;

    @Before
    public void setUp()
    {
        p0 = Sphere.make();
        p1 = Sphere.make();
        p2 = Sphere.make();
    }

    private Ray makeRay(final Point... points)
    {
        final Ray ray = Ray.make();

        for (final Point point : points)
        {
            ray.add(point.dist, point.obj, point.type, point.normal);
        }

        assertTrue(Ray.checkRay(ray));

        return ray;
    }

    private void assertRayEquals(final Ray expected, final Ray res)
    {
        assertTrue(Ray.checkRay(res));
        assertEquals(expected.numPoints(), res.numPoints());

        for (int i = 0; i < expected.numPoints(); ++i)
        {
            final Point p0 = expected.getPoint(i);
            final Point p1 = res.getPoint(i);

            assertEquals(p0.dist, p1.dist, 0.0);
            assertSame(p0.obj, p1.obj);
            assertSame(p0.type, p1.type);
            assertEquals(p0.normal, p1.normal);
        }
    }

    @Test
    public void testNull()
    {
        final Ray ray0 = Ray.make();
        ray0.add(1.0, p0, Type.INTERSECT, null);

        Ray res = Ray.union(null, null);
        assertTrue(Ray.checkRay(res));
        assertNull(res);

        res = Ray.union(null, ray0);
        assertTrue(Ray.checkRay(res));
        assertEquals(1, res.numPoints());
        assertEquals(1.0, res.getPoint(0).dist, 0.0);

        res = Ray.union(ray0, null);
        assertTrue(Ray.checkRay(res));
        assertEquals(1, res.numPoints());
        assertEquals(1.0, res.getPoint(0).dist, 0.0);
    }

    @Test
    public void testNonOverlapping()
    {
        final Ray ray0 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null));

        final Ray ray1 = makeRay(
                Point.make(3.0, p1, Type.ENTER, null),
                Point.make(4.0, p1, Type.LEAVE, null));

        final Ray expected = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null),
                Point.make(3.0, p1, Type.ENTER, null),
                Point.make(4.0, p1, Type.LEAVE, null));

        Ray res = Ray.union(ray0, ray1);
        assertRayEquals(expected, res);

        res = Ray.union(ray1, ray0);
        assertRayEquals(expected, res);
    }
}
