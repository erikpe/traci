package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.InterpreterBase;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.model.material.Color;

public class InterpreterColorTest extends InterpreterBase
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
}
