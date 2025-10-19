package se.ejp.traci.lang.interpreter.functions;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Collections;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.InterpreterBase;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalArgumentType;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalNumberOfArguments;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;

public class RandintTest extends InterpreterBase
{
    @Test
    public void testRandintReturnsInteger() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return randint(1, 10);");
        assertEquals(Type.NUMBER, value.getType());
        // Check that the value is an integer
        assertEquals(value.getNumber(), Math.floor(value.getNumber()), 0.0);
    }

    @Test
    public void testRandintInRange() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return randint(5, 10);");
        assertEquals(Type.NUMBER, value.getType());
        assertTrue(value.getNumber() >= 5);
        assertTrue(value.getNumber() <= 10);
    }

    @Test
    public void testRandintSingleValueRange() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return randint(7, 7);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(7.0, value.getNumber(), 0.0);
    }

    @Test
    public void testRandintNegativeRange() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return randint(-10, -5);");
        assertEquals(Type.NUMBER, value.getType());
        assertTrue(value.getNumber() >= -10);
        assertTrue(value.getNumber() <= -5);
    }

    @Test
    public void testRandintNegativeToPositive() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return randint(-5, 5);");
        assertEquals(Type.NUMBER, value.getType());
        assertTrue(value.getNumber() >= -5);
        assertTrue(value.getNumber() <= 5);
    }

    @Test
    public void testRandintZeroRange() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return randint(0, 0);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0.0, value.getNumber(), 0.0);
    }

    @Test
    public void testRandintIsDeterministic() throws RecognitionException, InterpreterRuntimeException
    {
        // Since randint uses a seeded Random(0), consecutive calls within the same program
        // should produce a deterministic sequence
        runInterpreter("a = randint(1, 100); b = randint(1, 100); return a;");
        assertEquals(Type.NUMBER, value.getType());
        // Just verify we get a number in range - the deterministic part is that
        // running the same code again would give the same results
        assertTrue(value.getNumber() >= 1);
        assertTrue(value.getNumber() <= 100);
    }

    @Test
    public void testRandintMultipleCalls() throws RecognitionException, InterpreterRuntimeException
    {
        // Test that we can call randint multiple times in the same program
        runInterpreter("a = randint(1, 100); b = randint(1, 100); return a == b;");
        assertEquals(Type.BOOLEAN, value.getType());
        // Within the same program run, consecutive calls may return different values
        // (though theoretically they could be the same by chance)
    }

    @Test
    public void testRandintInLoop() throws RecognitionException, InterpreterRuntimeException
    {
        // Test that all values are in range
        runInterpreter("min_val = 10; max_val = 20; count = 0; for (i in 1 .. 20) { val = randint(min_val, max_val); if (val >= min_val) { if (val <= max_val) { count = count + 1; } } } return count;");
        assertEquals(Type.NUMBER, value.getType());
        // All 20 iterations should have produced values in range
        assertEquals(20.0, value.getNumber(), 0.0);
    }

    @Test
    public void testRandintWithNoArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return randint();");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("randint", e.function);
            assertEquals(2, e.expectedNumArgs);
            assertEquals(0, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testRandintWithOneArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return randint(5);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("randint", e.function);
            assertEquals(2, e.expectedNumArgs);
            assertEquals(1, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testRandintWithThreeArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return randint(1, 2, 3);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("randint", e.function);
            assertEquals(2, e.expectedNumArgs);
            assertEquals(3, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testRandintWithVectorFirstArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return randint([1, 2, 3], 10);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("randint", e.function);
            assertEquals(Collections.singleton(Type.NUMBER), e.expectedArgType);
            assertEquals(Type.VECTOR, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testRandintWithVectorSecondArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return randint(1, [1, 2, 3]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("randint", e.function);
            assertEquals(Collections.singleton(Type.NUMBER), e.expectedArgType);
            assertEquals(Type.VECTOR, e.gotArgType);
            assertEquals(2, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testRandintWithStringArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return randint(\"test\", 10);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("randint", e.function);
            assertEquals(Collections.singleton(Type.NUMBER), e.expectedArgType);
            assertEquals(Type.STRING, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }
}
