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

public class SinTest extends InterpreterBase
{
    @Test
    public void testSinPositiveValue() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sin(2.23);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0.790480222342, value.getNumber(), 1e-10);
    }

    @Test
    public void testSinPi() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sin(3.14159265358979323846);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0, value.getNumber(), 1e-10);
    }

    @Test
    public void testSinZero() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sin(0);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0, value.getNumber(), 1e-10);
    }

    @Test
    public void testSinNegativePiOverTwo() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sin(-3.14159265358979323846/2);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(-1, value.getNumber(), 1e-10);
    }

    @Test
    public void testSinPiOverTwo() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sin(3.14159265358979323846/2);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1, value.getNumber(), 1e-10);
    }

    @Test
    public void testSinPiOverSix() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sin(3.14159265358979323846/6);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0.5, value.getNumber(), 1e-10);
    }

    @Test
    public void testSinNegativeValue() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sin(-1.0);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(-0.8414709848078965, value.getNumber(), 1e-10);
    }

    @Test
    public void testSinLargeValue() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sin(100.0);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(-0.5063656411097588, value.getNumber(), 1e-10);
    }

    @Test
    public void testSinWithNoArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return sin();");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("sin", e.function);
            assertEquals(1, e.expectedNumArgs);
            assertEquals(0, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testSinWithTwoArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return sin(1, 2);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("sin", e.function);
            assertEquals(1, e.expectedNumArgs);
            assertEquals(2, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testSinWithVectorArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return sin([1, 2, 3]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("sin", e.function);
            assertEquals(Collections.singleton(Type.NUMBER), e.expectedArgType);
            assertEquals(Type.VECTOR, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testSinWithStringArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return sin(\"test\");");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("sin", e.function);
            assertEquals(Collections.singleton(Type.NUMBER), e.expectedArgType);
            assertEquals(Type.STRING, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }
}
