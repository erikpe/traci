package se.ejp.traci.lang.interpreter.functions;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.Collections;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.InterpreterBase;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalArgumentType;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalNumberOfArguments;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;

public class LengthTest extends InterpreterBase
{
    @Test
    public void testLengthOfSimpleVector() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return length([1, 2, 3]);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(3.7416573867739, value.getNumber(), 1e-10);
    }

    @Test
    public void testLengthWithNegativeComponents() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return length([1, -2, 3]);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(3.7416573867739, value.getNumber(), 1e-10);
    }

    @Test
    public void testLengthOfZeroVector() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return length([0, 0, 0]);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0, value.getNumber(), 1e-10);
    }

    @Test
    public void testLengthOfUnitVectorX() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return length([1, 0, 0]);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testLengthOfUnitVectorY() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return length([0, 1, 0]);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testLengthOfUnitVectorZ() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return length([0, 0, 1]);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testLengthOfNegativeVector() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return length([-3, -4, 0]);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(5.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testLengthOf345Triangle() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return length([3, 4, 0]);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(5.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testLengthOfLargeVector() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return length([100, 0, 0]);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(100.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testLengthOfDecimalVector() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return length([0.5, 0.5, 0.707106781186547524400844362104849]);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testLengthInExpression() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return length([3, 4, 0]) * 2;");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(10.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testLengthOfVectorExpression() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("v = [1, 2, 3]; return length(v);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(3.7416573867739, value.getNumber(), 1e-10);
    }

    @Test
    public void testLengthWithNoArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return length();");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("length", e.function);
            assertEquals(1, e.expectedNumArgs);
            assertEquals(0, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testLengthWithTwoArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return length([1, 2, 3], [4, 5, 6]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("length", e.function);
            assertEquals(1, e.expectedNumArgs);
            assertEquals(2, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testLengthWithNumberArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return length(23);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("length", e.function);
            assertEquals(Collections.singleton(Type.VECTOR), e.expectedArgType);
            assertEquals(Type.NUMBER, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testLengthWithStringArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return length(\"test\");");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("length", e.function);
            assertEquals(Collections.singleton(Type.VECTOR), e.expectedArgType);
            assertEquals(Type.STRING, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }
}
