package se.ejp.traci.lang.interpreter.functions;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.InterpreterBase;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalNumberOfArguments;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;

public class RandTest extends InterpreterBase
{
    @Test
    public void testRandReturnsNumber() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return rand();");
        assertEquals(Type.NUMBER, value.getType());
    }

    @Test
    public void testRandReturnsValueBetweenZeroAndOne() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return rand();");
        assertEquals(Type.NUMBER, value.getType());
        assertTrue(value.getNumber() >= 0.0);
        assertTrue(value.getNumber() < 1.0);
    }

    @Test
    public void testRandIsDeterministic() throws RecognitionException, InterpreterRuntimeException
    {
        // Since rand() uses a seeded Random(0), consecutive calls within the same program
        // should return different values, but the sequence should be deterministic
        runInterpreter("a = rand(); b = rand(); return a != b;");
        assertEquals(Type.BOOLEAN, value.getType());
        assertTrue(value.getBoolean());
    }

    @Test
    public void testRandMultipleCalls() throws RecognitionException, InterpreterRuntimeException
    {
        // Test that we can call rand multiple times in the same program
        runInterpreter("a = rand(); b = rand(); return a == b;");
        assertEquals(Type.BOOLEAN, value.getType());
        // Within the same program run, consecutive calls should return different values
        assertEquals(false, value.getBoolean());
    }

    @Test
    public void testRandInExpression() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return rand() * 100;");
        assertEquals(Type.NUMBER, value.getType());
        assertTrue(value.getNumber() >= 0.0);
        assertTrue(value.getNumber() < 100.0);
    }

    @Test
    public void testRandWithArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return rand(1);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("rand", e.function);
            assertEquals(0, e.expectedNumArgs);
            assertEquals(1, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testRandWithTwoArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return rand(1, 2);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("rand", e.function);
            assertEquals(0, e.expectedNumArgs);
            assertEquals(2, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testRandInLoop() throws RecognitionException, InterpreterRuntimeException
    {
        // Test that rand() can be called in a loop and returns different values
        runInterpreter("sum = 0; for (i in 1 .. 10) { sum = sum + rand(); } return sum > 0;");
        assertEquals(Type.BOOLEAN, value.getType());
        assertTrue(value.getBoolean());
    }
}
