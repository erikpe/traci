package se.ejp.traci.lang.interpreter;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.TraciValue.Type;
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
}
