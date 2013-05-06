package se.ejp.traci.math;

import static org.junit.Assert.assertEquals;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import se.ejp.traci.math.Matrix;

public class TestMatrix
{
    @Before
    public void setUp() throws Exception
    {
    }

    @After
    public void tearDown() throws Exception
    {
    }

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
}
