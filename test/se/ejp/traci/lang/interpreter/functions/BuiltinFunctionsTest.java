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

public class BuiltinFunctionsTest extends InterpreterBase
{
    @Test
    public void testSin() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return sin(2.23);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0.790480222342, value.getNumber(), 1e-10);

        runInterpreter("return sin(3.14159265358979323846);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0, value.getNumber(), 1e-10);

        runInterpreter("return sin(0);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0, value.getNumber(), 1e-10);

        runInterpreter("return sin(-3.14159265358979323846/2);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(-1, value.getNumber(), 1e-10);

        try
        {
            runInterpreter("return sin(1, 2);");
            fail("Missed exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("sin", e.function);
            assertEquals(1, e.expectedNumArgs);
            assertEquals(2, e.gotNumArgs);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }

        try
        {
            runInterpreter("return sin([1, 2, 3]);");
            fail("Missed exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("sin", e.function);
            assertEquals(Collections.singleton(Type.NUMBER), e.expectedArgType);
            assertEquals(Type.VECTOR, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }
    }

    @Test
    public void testCos() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cos(2.23);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(-0.61248756565839, value.getNumber(), 1e-10);

        runInterpreter("return cos(3.14159265358979323846);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(-1, value.getNumber(), 1e-10);

        runInterpreter("return cos(0);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1, value.getNumber(), 1e-10);

        runInterpreter("return cos(-3.14159265358979323846/2);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0, value.getNumber(), 1e-10);

        try
        {
            runInterpreter("return cos(1, 2);");
            fail("Missed exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("cos", e.function);
            assertEquals(1, e.expectedNumArgs);
            assertEquals(2, e.gotNumArgs);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }

        try
        {
            runInterpreter("return cos([1, 2, 3]);");
            fail("Missed exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("cos", e.function);
            assertEquals(Collections.singleton(Type.NUMBER), e.expectedArgType);
            assertEquals(Type.VECTOR, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }
    }
}
