package se.ejp.traci.render;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import se.ejp.traci.render.Point.Type;

public class RayUnionTest extends RayBase
{
    @Test
    public void testNull()
    {
        final Ray ray0 = makeRay(
                Point.make(1.0, p0, Type.INTERSECT, null));

        final Ray expected = makeRay(
                Point.make(1.0, p0, Type.INTERSECT, null));

        Ray res = Ray.union(null, null);
        assertTrue(Ray.checkRay(res));
        assertNull(res);

        res = Ray.union(null, ray0);
        assertRayEquals(expected, res);

        res = Ray.union(ray0, null);
        assertRayEquals(expected, res);
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

    @Test
    public void testInterleaved()
    {
        final Ray ray0 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null),
                Point.make(5.0, p1, Type.ENTER, null),
                Point.make(6.0, p1, Type.LEAVE, null));

        final Ray ray1 = makeRay(
                Point.make(3.0, p2, Type.ENTER, null),
                Point.make(4.0, p2, Type.LEAVE, null),
                Point.make(7.0, p3, Type.ENTER, null),
                Point.make(8.0, p3, Type.LEAVE, null));

        final Ray expected = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null),
                Point.make(3.0, p2, Type.ENTER, null),
                Point.make(4.0, p2, Type.LEAVE, null),
                Point.make(5.0, p1, Type.ENTER, null),
                Point.make(6.0, p1, Type.LEAVE, null),
                Point.make(7.0, p3, Type.ENTER, null),
                Point.make(8.0, p3, Type.LEAVE, null));

        Ray res = Ray.union(ray0, ray1);
        assertRayEquals(expected, res);

        res = Ray.union(ray1, ray0);
        assertRayEquals(expected, res);
    }

    /*
     * ray0: | p0 |
     * ray1:    | p1 |
     */
    @Test
    public void testOverlapping1()
    {
        final Ray ray0 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null));

        final Ray ray1 = makeRay(
                Point.make(1.5, p1, Type.ENTER, null),
                Point.make(2.5, p1, Type.LEAVE, null));

        final Ray expected = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.5, p1, Type.LEAVE, null));

        Ray res = Ray.union(ray0, ray1);
        assertRayEquals(expected, res);

        res = Ray.union(ray1, ray0);
        assertRayEquals(expected, res);
    }

    /*
     * ray0: |   p0   |
     * ray1:   | p1 |
     */
    @Test
    public void testOverlapping2()
    {
        final Ray ray0 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(4.0, p0, Type.LEAVE, null));

        final Ray ray1 = makeRay(
                Point.make(2.0, p1, Type.ENTER, null),
                Point.make(3.0, p1, Type.LEAVE, null));

        final Ray expected = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(4.0, p0, Type.LEAVE, null));

        Ray res = Ray.union(ray0, ray1);
        assertRayEquals(expected, res);

        res = Ray.union(ray1, ray0);
        assertRayEquals(expected, res);
    }

    /*
     * ray0: | p0 |    | p1 |
     * ray1:    |   p2   |
     */
    @Test
    public void testOverlapping3()
    {
        final Ray ray0 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null),
                Point.make(3.0, p1, Type.ENTER, null),
                Point.make(4.0, p1, Type.LEAVE, null));

        final Ray ray1 = makeRay(
                Point.make(1.5, p2, Type.ENTER, null),
                Point.make(3.5, p2, Type.LEAVE, null));

        final Ray expected = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(4.0, p1, Type.LEAVE, null));

        Ray res = Ray.union(ray0, ray1);
        assertRayEquals(expected, res);

        res = Ray.union(ray1, ray0);
        assertRayEquals(expected, res);
    }

    /*
     * ray0: | p0 |    | p1 |
     * ray1:    |      p2      |
     */
    @Test
    public void testOverlapping4()
    {
        final Ray ray0 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null),
                Point.make(3.0, p1, Type.ENTER, null),
                Point.make(4.0, p1, Type.LEAVE, null));

        final Ray ray1 = makeRay(
                Point.make(1.5, p2, Type.ENTER, null),
                Point.make(4.5, p2, Type.LEAVE, null));

        final Ray expected = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(4.5, p2, Type.LEAVE, null));

        Ray res = Ray.union(ray0, ray1);
        assertRayEquals(expected, res);

        res = Ray.union(ray1, ray0);
        assertRayEquals(expected, res);
    }

    /*
     * ray0:   | p0 |    | p1 |
     * ray1: |      p2      |
     */
    @Test
    public void testOverlapping5()
    {
        final Ray ray0 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null),
                Point.make(3.0, p1, Type.ENTER, null),
                Point.make(4.0, p1, Type.LEAVE, null));

        final Ray ray1 = makeRay(
                Point.make(0.5, p2, Type.ENTER, null),
                Point.make(3.5, p2, Type.LEAVE, null));

        final Ray expected = makeRay(
                Point.make(0.5, p2, Type.ENTER, null),
                Point.make(4.0, p1, Type.LEAVE, null));

        Ray res = Ray.union(ray0, ray1);
        assertRayEquals(expected, res);

        res = Ray.union(ray1, ray0);
        assertRayEquals(expected, res);
    }

    /*
     * ray0:   | p0 |    | p1 |
     * ray1: |        p2        |
     */
    @Test
    public void testOverlapping6()
    {
        final Ray ray0 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null),
                Point.make(3.0, p1, Type.ENTER, null),
                Point.make(4.0, p1, Type.LEAVE, null));

        final Ray ray1 = makeRay(
                Point.make(0.5, p2, Type.ENTER, null),
                Point.make(4.5, p2, Type.LEAVE, null));

        final Ray expected = makeRay(
                Point.make(0.5, p2, Type.ENTER, null),
                Point.make(4.5, p2, Type.LEAVE, null));

        Ray res = Ray.union(ray0, ray1);
        assertRayEquals(expected, res);

        res = Ray.union(ray1, ray0);
        assertRayEquals(expected, res);
    }

    @Test
    public void testBig()
    {
        final Ray ray0 = Ray.make();
        final Ray ray1 = Ray.make();
        final Ray expected = Ray.make();

        for (int i = 0; i < 1000; ++i)
        {
            ray0.add(i, p0, Type.ENTER, null);
            ray0.add(i + 0.1, p0, Type.LEAVE, null);
            ray0.add(i + 0.2, p1, Type.INTERSECT, null);
            ray1.add(i + 0.05, p3, Type.INTERSECT, null);
            ray1.add(i + 0.3, p2, Type.ENTER, null);
            ray1.add(i + 0.4, p2, Type.LEAVE, null);

            expected.add(i, p0, Type.ENTER, null);
            expected.add(i + 0.1, p0, Type.LEAVE, null);
            expected.add(i + 0.2, p1, Type.INTERSECT, null);
            expected.add(i + 0.3, p2, Type.ENTER, null);
            expected.add(i + 0.4, p2, Type.LEAVE, null);
        }

        Ray res = Ray.union(ray0, ray1);
        assertRayEquals(expected, res);

        res = Ray.union(ray1, ray0);
        assertRayEquals(expected, res);
    }
}
