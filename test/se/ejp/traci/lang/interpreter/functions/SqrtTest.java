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

public class SqrtTest extends InterpreterBase
{
    @Test
    public void testSqrtOfPerfectSquare() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sqrt(4);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(2.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testSqrtOfNine() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sqrt(9);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(3.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testSqrtOfTwo() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sqrt(2);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1.4142135623730951, value.getNumber(), 1e-10);
    }

    @Test
    public void testSqrtOfZero() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sqrt(0);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testSqrtOfOne() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sqrt(1);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testSqrtOfLargeNumber() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sqrt(10000);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(100.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testSqrtOfDecimal() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sqrt(2.25);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1.5, value.getNumber(), 1e-10);
    }

    @Test
    public void testSqrtOfSmallDecimal() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sqrt(0.25);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0.5, value.getNumber(), 1e-10);
    }

    @Test
    public void testSqrtInExpression() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sqrt(16) + sqrt(9);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(7.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testSqrtOfSqrt() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sqrt(sqrt(16));");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(2.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testSqrtOfNegativeNumber() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sqrt(-1);");
        assertEquals(Type.NUMBER, value.getType());
        // sqrt of negative number returns NaN
        assertTrue(Double.isNaN(value.getNumber()));
    }

    @Test
    public void testSqrtWithNoArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return sqrt();");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("sqrt", e.function);
            assertEquals(1, e.expectedNumArgs);
            assertEquals(0, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testSqrtWithTwoArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return sqrt(4, 9);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("sqrt", e.function);
            assertEquals(1, e.expectedNumArgs);
            assertEquals(2, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testSqrtWithVectorArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return sqrt([1, 2, 3]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("sqrt", e.function);
            assertEquals(Collections.singleton(Type.NUMBER), e.expectedArgType);
            assertEquals(Type.VECTOR, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testSqrtWithStringArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return sqrt(\"test\");");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("sqrt", e.function);
            assertEquals(Collections.singleton(Type.NUMBER), e.expectedArgType);
            assertEquals(Type.STRING, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }
}
