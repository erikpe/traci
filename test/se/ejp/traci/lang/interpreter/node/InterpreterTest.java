package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.InterpreterBase;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.model.Color;
import se.ejp.traci.model.material.Interior;

public class InterpreterTest extends InterpreterBase
{
    @Test
    public void testColor() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return color [.1, .2, .3];");
        assertEquals(Type.COLOR, value.getType());
        Color color = value.getColor();
        assertEquals(0.1, color.r, 0);
        assertEquals(0.2, color.g, 0);
        assertEquals(0.3, color.b, 0);
        assertEquals(0.0, color.transmit, 0);

        runInterpreter("val = color [.1, .2, .3]; return val;");
        assertEquals(Type.COLOR, value.getType());
        color = value.getColor();
        assertEquals(0.1, color.r, 0);
        assertEquals(0.2, color.g, 0);
        assertEquals(0.3, color.b, 0);
        assertEquals(0.0, color.transmit, 0);

        runInterpreter("return color [.1, .2, .3, .4];");
        assertEquals(Type.COLOR, value.getType());
        color = value.getColor();
        assertEquals(0.1, color.r, 0);
        assertEquals(0.2, color.g, 0);
        assertEquals(0.3, color.b, 0);
        assertEquals(0.4, color.transmit, 0);

        runInterpreter("val = color [.1, .2, .3, .4]; return val;");
        assertEquals(Type.COLOR, value.getType());
        color = value.getColor();
        assertEquals(0.1, color.r, 0);
        assertEquals(0.2, color.g, 0);
        assertEquals(0.3, color.b, 0);
        assertEquals(0.4, color.transmit, 0);
    }

    @Test
    public void testInterior() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return interior(1.1);");
        assertEquals(Type.INTERIOR, value.getType());
        Interior interior = value.getInterior();
        assertEquals(1.1, interior.ior, 0);

        runInterpreter("val = interior(1.3); return val;");
        assertEquals(Type.INTERIOR, value.getType());
        interior = value.getInterior();
        assertEquals(1.3, interior.ior, 0);
    }
}
