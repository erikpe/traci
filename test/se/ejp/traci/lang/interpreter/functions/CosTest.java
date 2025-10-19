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

public class CosTest extends InterpreterBase
{
    @Test
    public void testCosPositiveValue() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cos(2.23);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(-0.61248756565839, value.getNumber(), 1e-10);
    }

    @Test
    public void testCosPi() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cos(3.14159265358979323846);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(-1, value.getNumber(), 1e-10);
    }

    @Test
    public void testCosZero() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cos(0);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1, value.getNumber(), 1e-10);
    }

    @Test
    public void testCosNegativePiOverTwo() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cos(-3.14159265358979323846/2);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0, value.getNumber(), 1e-10);
    }

    @Test
    public void testCosPiOverTwo() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cos(3.14159265358979323846/2);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0, value.getNumber(), 1e-10);
    }

    @Test
    public void testCosPiOverThree() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cos(3.14159265358979323846/3);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0.5, value.getNumber(), 1e-10);
    }

    @Test
    public void testCosNegativeValue() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cos(-1.0);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0.5403023058681398, value.getNumber(), 1e-10);
    }

    @Test
    public void testCosLargeValue() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cos(100.0);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0.8623188722876839, value.getNumber(), 1e-10);
    }

    @Test
    public void testCosTwoPi() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cos(2 * 3.14159265358979323846);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1, value.getNumber(), 1e-10);
    }

    @Test
    public void testCosWithNoArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return cos();");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("cos", e.function);
            assertEquals(1, e.expectedNumArgs);
            assertEquals(0, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testCosWithTwoArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return cos(1, 2);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("cos", e.function);
            assertEquals(1, e.expectedNumArgs);
            assertEquals(2, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testCosWithVectorArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return cos([1, 2, 3]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("cos", e.function);
            assertEquals(Collections.singleton(Type.NUMBER), e.expectedArgType);
            assertEquals(Type.VECTOR, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testCosWithStringArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return cos(\"test\");");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("cos", e.function);
            assertEquals(Collections.singleton(Type.NUMBER), e.expectedArgType);
            assertEquals(Type.STRING, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }
}
