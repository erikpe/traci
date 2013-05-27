package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIOException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.math.Projection2D;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.Color;
import se.ejp.traci.model.material.pigment.Checker;
import se.ejp.traci.model.material.pigment.FileImage;
import se.ejp.traci.model.material.pigment.RepeatPolicy;
import se.ejp.traci.model.material.pigment.Solid;

public class InterpreterPigmentTest extends InterpreterObjectBase
{
    public InterpreterPigmentTest()
    {
        super(Type.PIGMENT);
    }

    @Test
    public void testSolid() throws RecognitionException, InterpreterRuntimeException
    {
        final String args = "color [.1, .2, .3]";
        final String modifiers = "rotx 2.23;";

        for (final Solid solid : runTests("solid", Solid.class, args, null))
        {
            assertEquals(Color.make(.1, .2, .3), solid.getColor(Vector.ORIGO));
        }

        for (final Solid solid: runTests("solid", Solid.class, args, modifiers))
        {
            assertEquals(Color.make(.1, .2, .3), solid.getColor(Vector.ORIGO));
        }
    }

    @Test
    public void testChecker() throws RecognitionException, InterpreterRuntimeException
    {
        final String args = "color [.5, .5, .5], color [.1, .2, .3]";
        final String modifiers = "rotx 2.23;";

        for (final Checker checker : runTests("checker", Checker.class, args, null))
        {
            assertEquals(Color.make(.5, .5, .5), checker.color1.color);
            assertEquals(Color.make(.1, .2, .3), checker.color2.color);
            assertEquals(Transformations.identity(), checker.getTransformation());
        }

        for (final Checker checker: runTests("checker", Checker.class, args, modifiers))
        {
            assertEquals(Color.make(.5, .5, .5), checker.color1.color);
            assertEquals(Color.make(.1, .2, .3), checker.color2.color);
            assertEquals(Transformations.rotx(2.23), checker.getTransformation());
        }
    }

    @Test
    public void testImage() throws RecognitionException, InterpreterRuntimeException
    {
        final String args1 = "\"images/lenna.png\", \"repeat\", \"cylinder\"";
        final String args2 = "\"images/lenna.png\", \"border\", \"xy\", color [1,1,1]";
        final String modifiers = "rotx 2.23;";

        for (final FileImage fileImage : runTests("image", FileImage.class, args1, null))
        {
            assertEquals("images/lenna.png", fileImage.filename);
            assertEquals(RepeatPolicy.REPEAT, fileImage.repeatPolicy);
            assertEquals(Projection2D.CYLINDER, fileImage.projection);
            assertEquals(Color.BLACK, fileImage.borderColor);
            assertEquals(Transformations.identity(), fileImage.getTransformation());
        }

        for (final FileImage fileImage : runTests("image", FileImage.class, args2, modifiers))
        {
            assertEquals("images/lenna.png", fileImage.filename);
            assertEquals(RepeatPolicy.BORDER, fileImage.repeatPolicy);
            assertEquals(Projection2D.XY_PLANE, fileImage.projection);
            assertEquals(Color.WHITE, fileImage.borderColor);
            assertEquals(Transformations.rotx(2.23), fileImage.getTransformation());
        }

        runTestsFail(InterpreterIOException.class, "image", "\"non/existing/file.png\", \"repeat\", \"cylinder\"", null);
        runTestsFail(InterpreterRuntimeException.class, "image", "\"images/lenna.png\", \"knas\", \"cylinder\"", null);
        runTestsFail(InterpreterRuntimeException.class, "image", "\"images/lenna.png\", \"repeat\", \"knas\"", null);
    }
}
