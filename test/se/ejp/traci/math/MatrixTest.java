package se.ejp.traci.math;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class MatrixTest
{
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

    @Test
    public final void testMatrix()
    {
        final Vector v0 = Vector.make(1.0, 2.3, 3.0);
        final Vector v1 = Vector.make(-v0.x(), -v0.y(), -v0.z());
        final Vector v2 = Vector.make(1.0 / v0.x(), 1.0 / v0.y(), 1.0 / v0.z());

        assertEquals(Matrix.eye(), Matrix.eye().mul(Matrix.eye()));
        assertEquals(Matrix.eye(), Matrix.rotx(2.23).mul(Matrix.rotx(-2.23)));
        assertEquals(Matrix.eye(), Matrix.roty(3.23).mul(Matrix.roty(-3.23)));
        assertEquals(Matrix.eye(), Matrix.rotz(4.23).mul(Matrix.rotz(-4.23)));
        assertEquals(Matrix.eye(), Matrix.scale(v0).mul(Matrix.scale(v2)));
        assertEquals(Matrix.eye(), Matrix.translate(v0).mul(Matrix.translate(v1)));
    }
}
