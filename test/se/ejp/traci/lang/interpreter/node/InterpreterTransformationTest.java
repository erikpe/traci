package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Transformations;

public class InterpreterTransformationTest extends InterpreterObjectBase
{
    public InterpreterTransformationTest()
    {
        super(Type.TRANSFORMATION);
    }

    private List<Transformation> runTests(final String id, final String args, final String modifiers,
            final Transformation tr) throws RecognitionException, InterpreterRuntimeException
    {
        final List<Transformation> res = runTests(id, Transformation.class, args, modifiers);
        for (final Transformation transformation : res)
        {
            assertEquals(tr, transformation);
        }
        return res;
    }

    @Test
    public void testIdentity() throws RecognitionException, InterpreterRuntimeException
    {
        final String modifiers = "rotx 2.23; scale .5;";

        final Transformation tr0 = Transformations.rotx(2.23);
        final Transformation tr1 = Transformations.scale(0.5);

        runTests("identity", null, null, Transformations.identity());
        runTests("identity", null, modifiers, tr0.compose(tr1));
    }

    @Test
    public void testTranslate() throws RecognitionException, InterpreterRuntimeException
    {
        final String args0 = "[1.0, 10.0, 100.0]";
        final String args1 = "1.0, 10.0, 100.0";
        final String modifiers = "rotx 2.23; scale .5;";

        final Transformation tr0 = Transformations.translate(1.0, 10.0, 100.0);
        final Transformation tr1 = Transformations.rotx(2.23);
        final Transformation tr2 = Transformations.scale(0.5);

        runTests("translate", args0, null, tr0);
        runTests("translate", args1, null, tr0);
        runTests("translate", args0, modifiers, tr0.compose(tr1).compose(tr2));
        runTests("translate", args1, modifiers, tr0.compose(tr1).compose(tr2));
    }

    @Test
    public void testScale() throws RecognitionException, InterpreterRuntimeException
    {
        final String args0 = "0.5";
        final String args1 = "[2.0, 3.0, 4.0]";
        final String args2 = "2.0, 3.0, 4.0";
        final String modifiers = "rotx 2.23; translate [1, 1, 1];";

        final Transformation tr0 = Transformations.scale(0.5);
        final Transformation tr1 = Transformations.scale(2.0, 3.0, 4.0);
        final Transformation tr2 = Transformations.rotx(2.23);
        final Transformation tr3 = Transformations.translate(1.0, 1.0, 1.0);

        runTests("scale", args0, null, tr0);
        runTests("scale", args1, null, tr1);
        runTests("scale", args2, null, tr1);
        runTests("scale", args0, modifiers, tr0.compose(tr2).compose(tr3));
        runTests("scale", args1, modifiers, tr1.compose(tr2).compose(tr3));
        runTests("scale", args2, modifiers, tr1.compose(tr2).compose(tr3));

        runTests("scalex", args0, null,      Transformations.scalex(0.5));
        runTests("scalex", args0, modifiers, Transformations.scalex(0.5).compose(tr2).compose(tr3));

        runTests("scaley", args0, null,      Transformations.scaley(0.5));
        runTests("scaley", args0, modifiers, Transformations.scaley(0.5).compose(tr2).compose(tr3));

        runTests("scalez", args0, null,      Transformations.scalez(0.5));
        runTests("scalez", args0, modifiers, Transformations.scalez(0.5).compose(tr2).compose(tr3));
    }

    @Test
    public void testRot() throws RecognitionException, InterpreterRuntimeException
    {
        final String args0 = "2.23";
        final String modifiers = "scale .5; translate [1, 1, 1];";

        final Transformation trX = Transformations.rotx(2.23);
        final Transformation trY = Transformations.roty(2.23);
        final Transformation trZ = Transformations.rotz(2.23);
        final Transformation tr0 = Transformations.scale(0.5);
        final Transformation tr1 = Transformations.translate(1.0, 1.0, 1.0);

        runTests("rotx", args0, null, trX);
        runTests("roty", args0, null, trY);
        runTests("rotz", args0, null, trZ);

        runTests("rotx", args0, modifiers, trX.compose(tr0).compose(tr1));
        runTests("roty", args0, modifiers, trY.compose(tr0).compose(tr1));
        runTests("rotz", args0, modifiers, trZ.compose(tr0).compose(tr1));
    }
}
