package se.ejp.traci.render;

import org.junit.Test;

import se.ejp.traci.render.Point.Type;

public class RayIntersectionTest extends RayBase
{
    private void assertRayIntersect(final Ray expected, final Ray ray0, final Ray ray1)
    {
        for (int times = 1; times <= 3; ++times)
        {
            final Ray r0 = mulRay(ray0, times, 10.0);
            final Ray r1 = mulRay(ray1, times, 10.0);
            final Ray exp = mulRay(expected, times, 10.0);

            Ray res = Ray.intersect(r0, r1);
            assertRayEquals(exp, res);

            res = Ray.intersect(r1, r0);
            assertRayEquals(exp, res);
        }
    }

    @Test
    public void testNull()
    {
        final Ray ray0 = makeRay(Point.make(1.0, p0, Type.INTERSECT, null));

        assertRayIntersect(null, null, null);
        assertRayIntersect(null, ray0, null);
    }

    /*
     * ray0: | p0 |
     * ray1:         | p1 |
     */
    @Test
    public void testNonOverlapping()
    {
        final Ray ray0 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null));

        final Ray ray1 = makeRay(
                Point.make(3.0, p1, Type.ENTER, null),
                Point.make(4.0, p1, Type.LEAVE, null));

        assertRayIntersect(null, ray0, ray1);
    }

    /*
     * ray0: | p0 |        | p1 |
     * ray1:        | p2 |        | p3 |
     */
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

        assertRayIntersect(null, ray0, ray1);
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
                Point.make(1.5, p1, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null));

        assertRayIntersect(expected, ray0, ray1);
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
                Point.make(2.0, p1, Type.ENTER, null),
                Point.make(3.0, p1, Type.LEAVE, null));

        assertRayIntersect(expected, ray0, ray1);
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
                Point.make(1.5, p2, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null),
                Point.make(3.0, p1, Type.ENTER, null),
                Point.make(3.5, p2, Type.LEAVE, null));

        assertRayIntersect(expected, ray0, ray1);
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
                Point.make(1.5, p2, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null),
                Point.make(3.0, p1, Type.ENTER, null),
                Point.make(4.0, p1, Type.LEAVE, null));

        assertRayIntersect(expected, ray0, ray1);
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
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null),
                Point.make(3.0, p1, Type.ENTER, null),
                Point.make(3.5, p2, Type.LEAVE, null));

        assertRayIntersect(expected, ray0, ray1);
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
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null),
                Point.make(3.0, p1, Type.ENTER, null),
                Point.make(4.0, p1, Type.LEAVE, null));

        assertRayIntersect(expected, ray0, ray1);
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
            ray0.add(i + 0.7, p0, Type.LEAVE, null);
            ray0.add(i + 0.8, p1, Type.INTERSECT, null);
            ray0.add(i + 0.95, p3, Type.INTERSECT, null);

            ray1.add(i + 0.1, p2, Type.ENTER, null);
            ray1.add(i + 0.9, p2, Type.LEAVE, null);

            expected.add(i + 0.1, p2, Type.ENTER, null);
            expected.add(i + 0.7, p0, Type.LEAVE, null);
            expected.add(i + 0.8, p1, Type.INTERSECT, null);
        }

        Ray res = Ray.intersect(ray0, ray1);
        assertRayEquals(expected, res);

        res = Ray.intersect(ray1, ray0);
        assertRayEquals(expected, res);
    }
}
