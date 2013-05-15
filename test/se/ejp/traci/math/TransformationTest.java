package se.ejp.traci.math;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class TransformationTest
{
    @Test
    public void test()
    {
        final Transformation t1 = Transformations.translate(1.0, 2.5, 3.0);
        final Transformation t2 = Transformations.identity();

        final Transformation t3 = t1.compose(t2);
        final Transformation t4 = t2.compose(t1);

        assertEquals(t1, t3);
        assertEquals(t1, t4);
        assertEquals(t3, t4);
    }

    @Test
    public void test2()
    {
        final Transformation t1 = Transformations.translate(1.0, 2.5, 3.0);
        final Transformation t2 = Transformations.rotx(2.23);
        final Transformation t3 = Transformations.scaley(.45);

        final Transformation t4 = t1.compose(t2).compose(t3);
        final Transformation t5 = t1.compose(t2.compose(t3));

        assertEquals(t4, t5);
    }
}
