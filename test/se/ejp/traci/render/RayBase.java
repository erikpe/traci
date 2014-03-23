package se.ejp.traci.render;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import se.ejp.traci.model.shape.primitive.Primitive;
import se.ejp.traci.model.shape.primitive.Sphere;

public class RayBase
{
    protected final Primitive p0 = Sphere.make();
    protected final Primitive p1 = Sphere.make();
    protected final Primitive p2 = Sphere.make();
    protected final Primitive p3 = Sphere.make();
    protected final Primitive p4 = Sphere.make();
    protected final Primitive p5 = Sphere.make();

    protected Ray makeRay(final Point... points)
    {
        final Ray ray = Ray.make();

        for (final Point point : points)
        {
            ray.add(point.dist, point.obj, point.type, point.normal);
        }

        assertTrue(Ray.checkRay(ray));

        return ray;
    }

    protected void assertRayEquals(final Ray expected, final Ray res)
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
}
