package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalArguments;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.model.shape.BoundingBox;

public class InterpreterBoundingBoxTest extends InterpreterObjectBase
{
    public InterpreterBoundingBoxTest()
    {
        super(Type.BOUNDING_BOX);
    }

    private List<BoundingBox> runTests(final String args, final String modifiers, final Transformation tr)
            throws RecognitionException, InterpreterRuntimeException
    {
        final List<BoundingBox> res = runTests("bbox", BoundingBox.class, args, modifiers);
        for (final BoundingBox bBox : res)
        {
            assertEquals(tr, bBox.getTransformation());
        }
        return res;
    }

    @Test
    public void testBoundingBox() throws RecognitionException, InterpreterRuntimeException
    {
        final String args = "[1.0, 10.0, 100.0], [4.0, 14.0, 105.0]";
        final String modifiers = "translate([10.0, 20.0, 30.0]); scale(.5);";

        final Transformation tr0 = Transformations.scale(3.0, 4.0, 5.0);
        final Transformation tr1 = Transformations.translate(1.0, 10.0, 100.0);
        final Transformation tr2 = Transformations.translate(10.0, 20.0, 30.0);
        final Transformation tr3 = Transformations.scale(0.5);

        runTests(null, null,      Transformations.identity());
        runTests(args, null,      tr0.compose(tr1));
        runTests(null, modifiers, tr2.compose(tr3));
        runTests(args, modifiers, tr0.compose(tr1).compose(tr2).compose(tr3));

        runTestsFail(InterpreterIllegalArguments.class, "box", "[1.0, 10.0, 100.0]", null);
        runTestsFail(InterpreterIllegalArguments.class, "box", "[1.0, 10.0, 100.0], 23", null);
    }
}
