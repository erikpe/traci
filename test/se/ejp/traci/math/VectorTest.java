package se.ejp.traci.math;

import static org.junit.Assert.assertEquals;

import java.util.Random;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import se.ejp.traci.math.Vector;

public class VectorTest
{
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
            final long x = rand.nextLong();
            final long y = rand.nextLong();
            final long z = rand.nextLong();
            final Vector vec = Vector.make(x, y, z);
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
    public void testSub(){
        final Vector diff = v1.sub(v0);
        assertEquals(9, diff.x(), 0);
        assertEquals(18, diff.y(), 0);
        assertEquals(27, diff.z(), 0);
    }
}
