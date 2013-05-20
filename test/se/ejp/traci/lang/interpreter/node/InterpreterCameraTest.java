package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.Camera;

public class InterpreterCameraTest extends InterpreterObjectBase
{
    public InterpreterCameraTest()
    {
        super(Type.CAMERA);
    }

    private List<Camera> runTests(final String args, final String modifiers, final Transformation tr)
            throws RecognitionException, InterpreterRuntimeException
    {
        final List<Camera> res = runTests("camera", Camera.class, args, modifiers);
        for (final Camera camera : res)
        {
            assertEquals(tr, camera.getTransformation());
        }
        return res;
    }

    @Test
    public void testCamera() throws RecognitionException, InterpreterRuntimeException
    {
        final String args0 = "[1, 2, 3], [4, 5, 6]";
        final String args1 = "[1, 2, 3], [4, 5, 6], [1, 0, 0]";
        final String modifiers = "translate [1, 1, 1]; rotx 2.23;";

        final Vector v0 = Vector.make(1.0, 2.0, 3.0);
        final Vector v1 = Vector.make(4.0, 5.0, 6.0);

        final Transformation tr0 = Transformations.camera(v0, v1, Vector.UNIT_Y);
        final Transformation tr1 = Transformations.camera(v0, v1, Vector.UNIT_X);
        final Transformation tr2 = Transformations.translate(1.0, 1.0, 1.0);
        final Transformation tr3 = Transformations.rotx(2.23);

        runTests(args0, null, tr0);
        runTests(args0, modifiers, tr0.compose(tr2).compose(tr3));
        runTests(args1, modifiers, tr1.compose(tr2).compose(tr3));
    }
}
