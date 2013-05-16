package se.ejp.traci.math;

import static se.ejp.traci.util.AssertApprox.assertApprox;
import static se.ejp.traci.util.AssertNotApprox.assertNotApprox;

import org.junit.Test;

public class TransformationTest
{
    @Test
    public void testIdentity()
    {
        final Transformation t1 = Transformations.translate(1.0, 2.5, 3.0);
        final Transformation t2 = Transformations.identity();

        final Transformation t3 = t1.compose(t2);
        final Transformation t4 = t2.compose(t1);

        assertApprox(t1, t3);
        assertApprox(t1, t4);
        assertApprox(t3, t4);
    }

    @Test
    public void testAssociativity()
    {
        final Transformation t1 = Transformations.translate(1.0, 2.5, 3.0);
        final Transformation t2 = Transformations.rotx(2.23);
        final Transformation t3 = Transformations.scaley(.45);

        final Transformation t4 = t1.compose(t2).compose(t3);
        final Transformation t5 = t1.compose(t2.compose(t3));

        assertApprox(t4, t5);
    }

    @Test
    public void testNonCommutativity()
    {
        final Transformation t1 = Transformations.translate(1.0, 2.5, 3.0);
        final Transformation t2 = Transformations.rotx(2.23);

        final Transformation t3 = t1.compose(t2);
        final Transformation t4 = t2.compose(t1);

        assertNotApprox(t3, t4);
    }

    @Test
    public void testInvert()
    {
        final Transformation eye = Transformations.identity();
        final Vector v0 = Vector.make(1.0, 2.3, 3.0);
        final Vector v1 = Vector.make(-v0.x(), -v0.y(), -v0.z());
        final Vector v2 = Vector.make(1.0 / v0.x(), 1.0 / v0.y(), 1.0 / v0.z());
        final Vector v3 = Vector.make(1.5, 4.4, 5.0);

        assertApprox(eye, Transformations.translate(v0).compose(Transformations.translate(v1)));
        assertApprox(eye, Transformations.scale(v0).compose(Transformations.scale(v2)));
        assertApprox(eye, Transformations.scalex(2.23).compose(Transformations.scalex(1.0 / 2.23)));
        assertApprox(eye, Transformations.scaley(3.23).compose(Transformations.scaley(1.0 / 3.23)));
        assertApprox(eye, Transformations.scalez(4.23).compose(Transformations.scalez(1.0 / 4.23)));
        assertApprox(eye, Transformations.rotx(2.23).compose(Transformations.rotx(-2.23)));
        assertApprox(eye, Transformations.roty(3.23).compose(Transformations.roty(-3.23)));
        assertApprox(eye, Transformations.rotz(4.23).compose(Transformations.rotz(-4.23)));
        assertApprox(eye, Transformations.rotVecToZ(v0).compose(Transformations.rotZToVec(v0)), 1e-14);
        assertApprox(eye, Transformations.rotZToVec(v0).compose(Transformations.rotVecToZ(v0)), 1e-14);
        assertApprox(eye, Transformations.rotVecToVec(v0, v3).compose(Transformations.rotVecToVec(v3, v0)), 1e-14);
        assertApprox(eye, Transformations.rotAround(v0, v3, 2.23).compose(Transformations.rotAround(v0, v3, -2.23)), 1e-14);
        assertApprox(eye, Transformations.rotAround(v0, v3, 2.23).compose(Transformations.rotAround(v3, v0, 2.23)), 1e-14);
    }
}
