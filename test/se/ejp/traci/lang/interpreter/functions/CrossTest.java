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

public class CrossTest extends InterpreterBase
{
    @Test
    public void testCrossProductOfUnitVectorsXY() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cross([1, 0, 0], [0, 1, 0]);");
        assertEquals(Type.VECTOR, value.getType());
        assertEquals(0.0, value.getVector().x(), 1e-10);
        assertEquals(0.0, value.getVector().y(), 1e-10);
        assertEquals(1.0, value.getVector().z(), 1e-10);
    }

    @Test
    public void testCrossProductOfUnitVectorsYZ() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cross([0, 1, 0], [0, 0, 1]);");
        assertEquals(Type.VECTOR, value.getType());
        assertEquals(1.0, value.getVector().x(), 1e-10);
        assertEquals(0.0, value.getVector().y(), 1e-10);
        assertEquals(0.0, value.getVector().z(), 1e-10);
    }

    @Test
    public void testCrossProductOfUnitVectorsZX() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cross([0, 0, 1], [1, 0, 0]);");
        assertEquals(Type.VECTOR, value.getType());
        assertEquals(0.0, value.getVector().x(), 1e-10);
        assertEquals(1.0, value.getVector().y(), 1e-10);
        assertEquals(0.0, value.getVector().z(), 1e-10);
    }

    @Test
    public void testCrossProductOfParallelVectors() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cross([1, 0, 0], [2, 0, 0]);");
        assertEquals(Type.VECTOR, value.getType());
        assertEquals(0.0, value.getVector().x(), 1e-10);
        assertEquals(0.0, value.getVector().y(), 1e-10);
        assertEquals(0.0, value.getVector().z(), 1e-10);
    }

    @Test
    public void testCrossProductOfIdenticalVectors() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cross([3, 4, 5], [3, 4, 5]);");
        assertEquals(Type.VECTOR, value.getType());
        assertEquals(0.0, value.getVector().x(), 1e-10);
        assertEquals(0.0, value.getVector().y(), 1e-10);
        assertEquals(0.0, value.getVector().z(), 1e-10);
    }

    @Test
    public void testCrossProductSimple() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cross([1, 2, 3], [4, 5, 6]);");
        assertEquals(Type.VECTOR, value.getType());
        // i: 2*6 - 3*5 = 12 - 15 = -3
        // j: 3*4 - 1*6 = 12 - 6 = 6
        // k: 1*5 - 2*4 = 5 - 8 = -3
        assertEquals(-3.0, value.getVector().x(), 1e-10);
        assertEquals(6.0, value.getVector().y(), 1e-10);
        assertEquals(-3.0, value.getVector().z(), 1e-10);
    }

    @Test
    public void testCrossProductWithNegatives() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cross([1, -2, 3], [4, 5, -6]);");
        assertEquals(Type.VECTOR, value.getType());
        // i: (-2)*(-6) - 3*5 = 12 - 15 = -3
        // j: 3*4 - 1*(-6) = 12 + 6 = 18
        // k: 1*5 - (-2)*4 = 5 + 8 = 13
        assertEquals(-3.0, value.getVector().x(), 1e-10);
        assertEquals(18.0, value.getVector().y(), 1e-10);
        assertEquals(13.0, value.getVector().z(), 1e-10);
    }

    @Test
    public void testCrossProductAnticommutative() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("v1 = cross([1, 2, 3], [4, 5, 6]); v2 = cross([4, 5, 6], [1, 2, 3]); return v1 + v2;");
        assertEquals(Type.VECTOR, value.getType());
        // cross(a, b) = -cross(b, a), so v1 + v2 should be zero vector
        assertEquals(0.0, value.getVector().x(), 1e-10);
        assertEquals(0.0, value.getVector().y(), 1e-10);
        assertEquals(0.0, value.getVector().z(), 1e-10);
    }

    @Test
    public void testCrossProductOrthogonalToInputs() throws RecognitionException, InterpreterRuntimeException
    {
        // Cross product should be orthogonal to both input vectors
        // Test: dot(cross(a, b), a) == 0 and dot(cross(a, b), b) == 0
        runInterpreter("a = [1, 2, 3]; b = [4, 5, 6]; c = cross(a, b); return dot(c, a);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0.0, value.getNumber(), 1e-10);

        runInterpreter("a = [1, 2, 3]; b = [4, 5, 6]; c = cross(a, b); return dot(c, b);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0.0, value.getNumber(), 1e-10);
    }

    @Test
    public void testCrossProductWithZeroVector() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cross([0, 0, 0], [1, 2, 3]);");
        assertEquals(Type.VECTOR, value.getType());
        assertEquals(0.0, value.getVector().x(), 1e-10);
        assertEquals(0.0, value.getVector().y(), 1e-10);
        assertEquals(0.0, value.getVector().z(), 1e-10);
    }

    @Test
    public void testCrossProductWithDecimals() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return cross([0.5, 1.5, 2.5], [2.0, 1.0, 0.5]);");
        assertEquals(Type.VECTOR, value.getType());
        // i: 1.5*0.5 - 2.5*1.0 = 0.75 - 2.5 = -1.75
        // j: 2.5*2.0 - 0.5*0.5 = 5.0 - 0.25 = 4.75
        // k: 0.5*1.0 - 1.5*2.0 = 0.5 - 3.0 = -2.5
        assertEquals(-1.75, value.getVector().x(), 1e-10);
        assertEquals(4.75, value.getVector().y(), 1e-10);
        assertEquals(-2.5, value.getVector().z(), 1e-10);
    }

    @Test
    public void testCrossProductWithVariables() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("v1 = [2, 3, 4]; v2 = [1, 0, 1]; return cross(v1, v2);");
        assertEquals(Type.VECTOR, value.getType());
        // i: 3*1 - 4*0 = 3
        // j: 4*1 - 2*1 = 2
        // k: 2*0 - 3*1 = -3
        assertEquals(3.0, value.getVector().x(), 1e-10);
        assertEquals(2.0, value.getVector().y(), 1e-10);
        assertEquals(-3.0, value.getVector().z(), 1e-10);
    }

    @Test
    public void testCrossWithNoArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return cross();");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("cross", e.function);
            assertEquals(2, e.expectedNumArgs);
            assertEquals(0, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testCrossWithOneArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return cross([1, 2, 3]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("cross", e.function);
            assertEquals(2, e.expectedNumArgs);
            assertEquals(1, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testCrossWithThreeArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return cross([1, 2, 3], [4, 5, 6], [7, 8, 9]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("cross", e.function);
            assertEquals(2, e.expectedNumArgs);
            assertEquals(3, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testCrossWithNumberFirstArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return cross(5, [1, 2, 3]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("cross", e.function);
            assertEquals(Collections.singleton(Type.VECTOR), e.expectedArgType);
            assertEquals(Type.NUMBER, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testCrossWithNumberSecondArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return cross([1, 2, 3], 5);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("cross", e.function);
            assertEquals(Collections.singleton(Type.VECTOR), e.expectedArgType);
            assertEquals(Type.NUMBER, e.gotArgType);
            assertEquals(2, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testCrossWithStringArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return cross(\"test\", [1, 2, 3]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("cross", e.function);
            assertEquals(Collections.singleton(Type.VECTOR), e.expectedArgType);
            assertEquals(Type.STRING, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(7, e.getLocation().fileLocation.col);
        }
    }
}
