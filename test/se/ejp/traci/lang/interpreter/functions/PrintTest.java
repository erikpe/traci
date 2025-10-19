package se.ejp.traci.lang.interpreter.functions;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import org.antlr.runtime.RecognitionException;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.InterpreterBase;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalNumberOfArguments;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;

public class PrintTest extends InterpreterBase
{
    private final ByteArrayOutputStream outContent = new ByteArrayOutputStream();
    private final PrintStream originalOut = System.out;

    @Before
    public void setUpStreams()
    {
        System.setOut(new PrintStream(outContent));
    }

    @After
    public void restoreStreams()
    {
        System.setOut(originalOut);
    }

    @Test
    public void testPrintNumber() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("print(42);");
        String output = outContent.toString();
        assertTrue(output.contains("<NUMBER:42.0>"));
        assertNull(value);
    }

    @Test
    public void testPrintNegativeNumber() throws RecognitionException, InterpreterRuntimeException
    {
        outContent.reset();
        runInterpreter("print(-3.14);");
        String output = outContent.toString();
        assertTrue(output.contains("<NUMBER:-3.14>"));
        assertNull(value);
    }

    @Test
    public void testPrintString() throws RecognitionException, InterpreterRuntimeException
    {
        outContent.reset();
        runInterpreter("print(\"Hello, World!\");");
        String output = outContent.toString();
        assertTrue(output.contains("<STRING:Hello, World!>"));
        assertNull(value);
    }

    @Test
    public void testPrintVector() throws RecognitionException, InterpreterRuntimeException
    {
        outContent.reset();
        runInterpreter("print([1, 2, 3]);");
        String output = outContent.toString();
        assertTrue(output.contains("<VECTOR:"));
        assertNull(value);
    }

    @Test
    public void testPrintExpressionResult() throws RecognitionException, InterpreterRuntimeException
    {
        outContent.reset();
        runInterpreter("print(2 + 2);");
        String output = outContent.toString();
        assertTrue(output.contains("<NUMBER:4.0>"));
        assertNull(value);
    }

    @Test
    public void testPrintWithNoArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("print();");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("print", e.function);
            assertEquals(1, e.expectedNumArgs);
            assertEquals(0, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(0, e.getLocation().fileLocation.col);
        }
    }

    @Test
    public void testPrintWithTooManyArguments() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("print(1, 2);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalNumberOfArguments e)
        {
            assertEquals("print", e.function);
            assertEquals(1, e.expectedNumArgs);
            assertEquals(2, e.gotNumArgs);
            assertEquals(1, e.getLocation().fileLocation.row);
            assertEquals(0, e.getLocation().fileLocation.col);
        }
    }
}
