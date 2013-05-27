package se.ejp.traci.lang.interpreter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalOperatorArgument;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;

public class InterpreterTest extends InterpreterBase
{
    @Test
    public void testFibonacci() throws RecognitionException, IOException, InterpreterRuntimeException
    {
        runInterpreterFile("testcode/fibonacci.traci");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(55, value.getNumber(), 0);
    }

    @Test
    public void testFunctionscope() throws RecognitionException, IOException, InterpreterRuntimeException
    {
        runInterpreterFile("testcode/nested-function.traci");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(25, value.getNumber(), 0);
    }

    @Test
    public void testGlobalValue() throws RecognitionException, IOException, InterpreterRuntimeException
    {
        runInterpreterFile("testcode/global-value.traci");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(44, value.getNumber(), 0);
    }

    @Test
    public void testWhileLoop() throws RecognitionException, IOException, InterpreterRuntimeException
    {
        runInterpreterFile("testcode/while-loop.traci");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(55, value.getNumber(), 0);
    }

    @Test
    public void testForLoop() throws RecognitionException, IOException, InterpreterRuntimeException
    {
        runInterpreterFile("testcode/for-loop.traci");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(45, value.getNumber(), 0);
    }

    @Test
    public void testIfStatement() throws RecognitionException, IOException, InterpreterRuntimeException
    {
        runInterpreterFile("testcode/if-statement.traci");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(14, value.getNumber(), 0);
    }

    @Test
    public void testFloat() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return .23;");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0.23, value.getNumber(), 0);

        runInterpreter("return 2.23;");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(2.23, value.getNumber(), 0);

        runInterpreter("return -3.23E+5;");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(-3.23E+5, value.getNumber(), 0);

        runInterpreter("return .23e-3;");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(.23e-3, value.getNumber(), 0);
    }

    @Test
    public void testAdd() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return 17+23;");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(40, value.getNumber(), 0);

        try
        {
            runInterpreter("17+(1<2);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalOperatorArgument e)
        {
            assertEquals(Type.NUMBER, e.leftType);
            assertEquals("+", e.op);
            assertEquals(Type.BOOLEAN, e.rightType);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(2, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testString() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return \"foo/bar.hej\";");
        assertEquals(Type.STRING, value.getType());
        assertEquals("foo/bar.hej", value.getString());
    }

    @Test
    public void test() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("rotz(360)*2;");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalOperatorArgument e)
        {
            assertEquals(Type.TRANSFORMATION, e.leftType);
            assertEquals("*", e.op);
            assertEquals(Type.NUMBER, e.rightType);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(9, e.getLocation().fileLocation.col);
        }
    }
}
