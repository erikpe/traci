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

public class DotTest extends InterpreterBase
{
    @Test
    public void testDotProductOfOrthogonalVectors() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return dot([1, 0, 0], [0, 1, 0]);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testDotProductOfParallelVectors() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return dot([1, 0, 0], [2, 0, 0]);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(2.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testDotProductOfIdenticalVectors() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return dot([3, 4, 0], [3, 4, 0]);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(25.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testDotProductSimple() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return dot([1, 2, 3], [4, 5, 6]);");
        assertEquals(Type.NUMBER, value.getType());
        // 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32
        assertEquals(32.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testDotProductWithNegatives() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return dot([1, -2, 3], [4, 5, -6]);");
        assertEquals(Type.NUMBER, value.getType());
        // 1*4 + (-2)*5 + 3*(-6) = 4 - 10 - 18 = -24
        assertEquals(-24.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testDotProductOfZeroVectors() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return dot([0, 0, 0], [1, 2, 3]);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testDotProductOfUnitVectors() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return dot([1, 0, 0], [1, 0, 0]);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testDotProductCommutative() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("a = dot([1, 2, 3], [4, 5, 6]); b = dot([4, 5, 6], [1, 2, 3]); return a == b;");
        assertEquals(Type.BOOLEAN, value.getType());
        assertEquals(true, value.getBoolean());
    }

    @Test
    public void testDotProductWithDecimals() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return dot([0.5, 1.5, 2.5], [2.0, 1.0, 0.5]);");
        assertEquals(Type.NUMBER, value.getType());
        // 0.5*2.0 + 1.5*1.0 + 2.5*0.5 = 1.0 + 1.5 + 1.25 = 3.75
        assertEquals(3.75, value.getNumber(), 1e-10);
    }

    @Test
    public void testDotProductInExpression() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return dot([1, 0, 0], [2, 0, 0]) * 3;");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(6.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testDotProductOfNegativeVectors() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return dot([-1, -2, -3], [1, 2, 3]);");
        assertEquals(Type.NUMBER, value.getType());
        // -1*1 + -2*2 + -3*3 = -1 - 4 - 9 = -14
        assertEquals(-14.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testDotProductWithVariables() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("v1 = [2, 3, 4]; v2 = [1, 0, 1]; return dot(v1, v2);");
        assertEquals(Type.NUMBER, value.getType());
        // 2*1 + 3*0 + 4*1 = 2 + 0 + 4 = 6
        assertEquals(6.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testDotWithNoArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return dot();");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("dot", e.function);
            assertEquals(2, e.expectedNumArgs);
            assertEquals(0, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testDotWithOneArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return dot([1, 2, 3]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("dot", e.function);
            assertEquals(2, e.expectedNumArgs);
            assertEquals(1, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testDotWithThreeArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return dot([1, 2, 3], [4, 5, 6], [7, 8, 9]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("dot", e.function);
            assertEquals(2, e.expectedNumArgs);
            assertEquals(3, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testDotWithNumberFirstArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return dot(5, [1, 2, 3]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("dot", e.function);
            assertEquals(Collections.singleton(Type.VECTOR), e.expectedArgType);
            assertEquals(Type.NUMBER, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testDotWithNumberSecondArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return dot([1, 2, 3], 5);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("dot", e.function);
            assertEquals(Collections.singleton(Type.VECTOR), e.expectedArgType);
            assertEquals(Type.NUMBER, e.gotArgType);
            assertEquals(2, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testDotWithStringArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return dot(\"test\", [1, 2, 3]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("dot", e.function);
            assertEquals(Collections.singleton(Type.VECTOR), e.expectedArgType);
            assertEquals(Type.STRING, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }
}
