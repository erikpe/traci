package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.Color;
import se.ejp.traci.model.light.AmbientLight;
import se.ejp.traci.model.light.PointLight;

public class InterpreterLightTest extends InterpreterObjectBase
{
    public InterpreterLightTest()
    {
        super(Type.LIGHT);
    }

    @Test
    public void testPointLight() throws RecognitionException, InterpreterRuntimeException
    {
        final String args = "[1, 2, 3], color [1, .5, .5]";
        final String modifiers = "rotx 2.23; translate [1, 1, 1];";

        final Transformation tr0 = Transformations.rotx(2.23);
        final Transformation tr1 = Transformations.translate(1.0, 1.0, 1.0);

        final Color color = Color.make(1.0, 0.5, 0.5);
        final Vector loc0 = Vector.make(1.0, 2.0, 3.0);
        final Vector loc1 = tr0.compose(tr1).point(loc0);

        for (final PointLight pointLight : runTests("pointlight", PointLight.class, args, null))
        {
            assertEquals(color, pointLight.getColor());
            assertEquals(loc0, pointLight.getLocation());
        }

        for (final PointLight pointLight : runTests("pointlight", PointLight.class, args, modifiers))
        {
            assertEquals(color, pointLight.getColor());
            assertEquals(loc1, pointLight.getLocation());
        }
    }

    @Test
    public void testAmbientLight() throws RecognitionException, InterpreterRuntimeException
    {
        final String args = "color [.1, .2, .3]";
        final String modifiers = "rotx 2.23; translate [1, 1, 1];";

        final Color color = Color.make(0.1, 0.2, 0.3);

        for (final AmbientLight ambientLight : runTests("ambientlight", AmbientLight.class, args, null))
        {
            assertEquals(color, ambientLight.getColor());
        }

        for (final AmbientLight ambientLight : runTests("ambientlight", AmbientLight.class, args, modifiers))
        {
            assertEquals(color, ambientLight.getColor());
        }
    }
}
