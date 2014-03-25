package se.ejp.traci.render;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import se.ejp.traci.render.Point.Type;

public class RayDifferenceTest extends RayBase
{
    private void assertRayDifference(final Ray expected, final Ray ray0, final Ray ray1)
    {
        assertTrue(Ray.checkRay(expected));
        assertTrue(Ray.checkRay(ray0));
        assertTrue(Ray.checkRay(ray1));

        for (int times = 1; times <= 3; ++times)
        {
            final Ray r0 = mulRay(ray0, times, 10.0);
            final Ray r1 = mulRay(ray1, times, 10.0);
            final Ray exp = mulRay(expected, times, 10.0);

            final Ray res = Ray.difference(r0, r1);
            assertRayEquals(exp, res);
        }
    }

    @Test
    public void testNull()
    {
        final Ray ray0 = makeRay(Point.make(1.0, p0, Type.INTERSECT, null));
        final Ray expected0 = makeRay(Point.make(1.0, p0, Type.INTERSECT, null));

        final Ray ray1 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null));

        final Ray expected1 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null));

        assertRayDifference(null, null, null);
        assertRayDifference(expected0, ray0, null);
        assertRayDifference(null, null, ray0);
        assertRayDifference(expected1, ray1, null);
        assertRayDifference(null, null, ray1);
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

        assertRayDifference(ray0, ray0, ray1);
        assertRayDifference(ray1, ray1, ray0);
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

        assertRayDifference(ray0, ray0, ray1);
        assertRayDifference(ray1, ray1, ray0);
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

        final Ray expected0 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(1.5, p1, Type.LEAVE, null));

        final Ray expected1 = makeRay(
                Point.make(2.0, p0, Type.ENTER, null),
                Point.make(2.5, p1, Type.LEAVE, null));

        assertRayDifference(expected0, ray0, ray1);
        assertRayDifference(expected1, ray1, ray0);
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
                Point.make(2.0, p1, Type.LEAVE, null),
                Point.make(3.0, p1, Type.ENTER, null),
                Point.make(4.0, p0, Type.LEAVE, null));

        assertRayDifference(expected, ray0, ray1);
        assertRayDifference(null, ray1, ray0);
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

        final Ray expected0 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(1.5, p2, Type.LEAVE, null),
                Point.make(3.5, p2, Type.ENTER, null),
                Point.make(4.0, p1, Type.LEAVE, null));

        final Ray expected1 = makeRay(
                Point.make(2.0, p0, Type.ENTER, null),
                Point.make(3.0, p1, Type.LEAVE, null));

        assertRayDifference(expected0, ray0, ray1);
        assertRayDifference(expected1, ray1, ray0);
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

        final Ray expected0 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(1.5, p2, Type.LEAVE, null));

        final Ray expected1 = makeRay(
                Point.make(2.0, p0, Type.ENTER, null),
                Point.make(3.0, p1, Type.LEAVE, null),
                Point.make(4.0, p1, Type.ENTER, null),
                Point.make(4.5, p2, Type.LEAVE, null));

        assertRayDifference(expected0, ray0, ray1);
        assertRayDifference(expected1, ray1, ray0);
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

        final Ray expected0 = makeRay(
                Point.make(3.5, p2, Type.ENTER, null),
                Point.make(4.0, p1, Type.LEAVE, null));

        final Ray expected1 = makeRay(
                Point.make(0.5, p2, Type.ENTER, null),
                Point.make(1.0, p0, Type.LEAVE, null),
                Point.make(2.0, p0, Type.ENTER, null),
                Point.make(3.0, p1, Type.LEAVE, null));

        assertRayDifference(expected0, ray0, ray1);
        assertRayDifference(expected1, ray1, ray0);
    }

    /*
     * ray0:   | p0 |  I  | p1 |
     * ray1: |        p2         |
     */
    @Test
    public void testOverlapping6()
    {
        final Ray ray0 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null),
                Point.make(2.5, p3, Type.INTERSECT, null),
                Point.make(3.0, p1, Type.ENTER, null),
                Point.make(4.0, p1, Type.LEAVE, null));

        final Ray ray1 = makeRay(
                Point.make(0.5, p2, Type.ENTER, null),
                Point.make(4.5, p2, Type.LEAVE, null));

        final Ray expected = makeRay(
                Point.make(0.5, p2, Type.ENTER, null),
                Point.make(1.0, p0, Type.LEAVE, null),
                Point.make(2.0, p0, Type.ENTER, null),
                Point.make(3.0, p1, Type.LEAVE, null),
                Point.make(4.0, p1, Type.ENTER, null),
                Point.make(4.5, p2, Type.LEAVE, null));

        assertRayDifference(null, ray0, ray1);
        assertRayDifference(expected, ray1, ray0);
    }

    /*
     * ray0:   | p0 |
     * ray1: I    I    I
     */
    @Test
    public void testOverlapping7()
    {
        final Ray ray0 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null));

        final Ray ray1 = makeRay(
                Point.make(0.5, p1, Type.INTERSECT, null),
                Point.make(1.5, p2, Type.INTERSECT, null),
                Point.make(2.5, p3, Type.INTERSECT, null));

        final Ray expected0 = makeRay(
                Point.make(1.0, p0, Type.ENTER, null),
                Point.make(2.0, p0, Type.LEAVE, null));

        final Ray expected1 = makeRay(
                Point.make(0.5, p1, Type.INTERSECT, null),
                Point.make(2.5, p3, Type.INTERSECT, null));

        assertRayDifference(expected0, ray0, ray1);
        assertRayDifference(expected1, ray1, ray0);
    }

    @Test
    public void testBig()
    {
        final Ray ray0 = Ray.make();
        final Ray ray1 = Ray.make();
        final Ray expected0 = Ray.make();
        final Ray expected1 = Ray.make();

        for (int i = 0; i < 2; ++i)
        {
            ray0.add(i, p0, Type.ENTER, null);
            ray0.add(i + 0.2, p0, Type.LEAVE, null);
            ray0.add(i + 0.3, p1, Type.INTERSECT, null);

            ray1.add(i + 0.1, p2, Type.ENTER, null);
            ray1.add(i + 0.4, p2, Type.LEAVE, null);
            ray1.add(i + 0.5, p3, Type.INTERSECT, null);

            expected0.add(i, p0, Type.ENTER, null);
            expected0.add(i + 0.1, p2, Type.LEAVE, null);

            expected1.add(i + 0.2, p0, Type.ENTER, null);
            expected1.add(i + 0.4, p2, Type.LEAVE, null);
            expected1.add(i + 0.5, p3, Type.INTERSECT, null);
        }

        assertTrue(Ray.checkRay(expected0));
        assertTrue(Ray.checkRay(expected1));
        assertTrue(Ray.checkRay(ray0));
        assertTrue(Ray.checkRay(ray1));

        Ray res = Ray.difference(ray0, ray1);
        assertRayEquals(expected0, res);

        res = Ray.difference(ray1, ray0);
        assertRayEquals(expected1, res);
    }
}
