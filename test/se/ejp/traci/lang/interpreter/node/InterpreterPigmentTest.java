package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.model.material.Color;
import se.ejp.traci.model.material.pigment.Checker;
import se.ejp.traci.model.material.pigment.FileImage;
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
            assertEquals(Color.make(.1, .2, .3), solid.color);
        }

        for (final Solid solid: runTests("solid", Solid.class, args, modifiers))
        {
            assertEquals(Color.make(.1, .2, .3), solid.color);
        }
    }

    @Test
    public void testChecker() throws RecognitionException, InterpreterRuntimeException
    {
        final String args = "color [.5, .5, .5], color [.1, .2, .3]";
        final String modifiers = "rotx 2.23;";

        for (final Checker checker : runTests("checker", Checker.class, args, null))
        {
            assertEquals(Color.make(.5, .5, .5), checker.color1);
            assertEquals(Color.make(.1, .2, .3), checker.color2);
            assertEquals(Transformations.identity(), checker.getTransformation());
        }

        for (final Checker checker: runTests("checker", Checker.class, args, modifiers))
        {
            assertEquals(Color.make(.5, .5, .5), checker.color1);
            assertEquals(Color.make(.1, .2, .3), checker.color2);
            assertEquals(Transformations.rotx(2.23), checker.getTransformation());
        }
    }

    @Test
    public void testImage() throws RecognitionException, InterpreterRuntimeException
    {
        final String args = "\"images/lenna.png\", \"repeat\", \"cylinder\"";
        final String modifiers = "rotx 2.23;";

        runTests("image", FileImage.class, args, null);
        runTests("image", FileImage.class, args, modifiers);
    }
}
