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
        assertEquals(1014 + 123, value.getNumber(), 0);
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

    @Test
    public void testPrimeCheckerSnippets() throws RecognitionException, IOException, InterpreterRuntimeException
    {
        loadInterpreterFile("testcode/prime-checker.traci");

        // Test small primes
        runSnippet("return isPrime(2);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1, value.getNumber(), 0);

        runSnippet("return isPrime(3);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1, value.getNumber(), 0);

        runSnippet("return isPrime(5);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1, value.getNumber(), 0);

        runSnippet("return isPrime(7);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1, value.getNumber(), 0);

        runSnippet("return isPrime(11);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1, value.getNumber(), 0);

        // Test larger primes
        runSnippet("return isPrime(23);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1, value.getNumber(), 0);

        runSnippet("return isPrime(97);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1, value.getNumber(), 0);

        // Test composite numbers
        runSnippet("return isPrime(1);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0, value.getNumber(), 0);

        runSnippet("return isPrime(4);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0, value.getNumber(), 0);

        runSnippet("return isPrime(6);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0, value.getNumber(), 0);

        runSnippet("return isPrime(9);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0, value.getNumber(), 0);

        runSnippet("return isPrime(15);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0, value.getNumber(), 0);

        runSnippet("return isPrime(20);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0, value.getNumber(), 0);

        runSnippet("return isPrime(100);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0, value.getNumber(), 0);

        // Test edge case
        runSnippet("return isPrime(0);");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(0, value.getNumber(), 0);
    }

    @Test
    public void testBooleanLiterals() throws RecognitionException, IOException, InterpreterRuntimeException
    {
        runInterpreterFile("testcode/boolean-literals.traci");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(6, value.getNumber(), 0);  // count should be 6
    }

    @Test
    public void testBooleanSimpleFile() throws RecognitionException, IOException, InterpreterRuntimeException
    {
        runInterpreterFile("testcode/boolean-simple.traci");
        assertEquals(Type.BOOLEAN, value.getType());
        assertEquals(true, value.getBoolean());
    }

    @Test
    public void testBooleanComprehensive() throws RecognitionException, IOException, InterpreterRuntimeException
    {
        // This file tests boolean literals in various contexts and uses print statements
        // It doesn't return a value, so we just verify it runs without errors
        runInterpreterFile("testcode/boolean-comprehensive.traci");
        // No return value expected, just verify no exceptions thrown
    }

    @Test
    public void testBooleanSimple() throws RecognitionException, InterpreterRuntimeException
    {
        // Test true literal
        runInterpreter("return true;");
        assertEquals(Type.BOOLEAN, value.getType());
        assertEquals(true, value.getBoolean());

        // Test false literal
        runInterpreter("return false;");
        assertEquals(Type.BOOLEAN, value.getType());
        assertEquals(false, value.getBoolean());

        // Test boolean in if
        runInterpreter("x = 0; if (true) { x = 1; } return x;");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1, value.getNumber(), 0);

        // Test boolean equality
        runInterpreter("return true == true;");
        assertEquals(Type.BOOLEAN, value.getType());
        assertEquals(true, value.getBoolean());

        runInterpreter("return true == false;");
        assertEquals(Type.BOOLEAN, value.getType());
        assertEquals(false, value.getBoolean());

        // Test boolean NOT
        runInterpreter("return !true;");
        assertEquals(Type.BOOLEAN, value.getType());
        assertEquals(false, value.getBoolean());

        runInterpreter("return !false;");
        assertEquals(Type.BOOLEAN, value.getType());
        assertEquals(true, value.getBoolean());
    }

    @Test
    public void testBooleanInControlFlow() throws RecognitionException, InterpreterRuntimeException
    {
        // Test while loop with boolean
        runInterpreter("count = 0; running = true; while (running) { count = count + 1; if (count > 3) { running = false; } } return count;");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(4, value.getNumber(), 0);

        // Test if-else with boolean literals
        runInterpreter("if (true) { return 1; } else { return 0; }");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1, value.getNumber(), 0);

        runInterpreter("if (false) { return 0; } else { return 1; }");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1, value.getNumber(), 0);

        // Test boolean from comparison
        runInterpreter("is_positive = (10 > 0); if (is_positive) { return 1; } return 0;");
        assertEquals(Type.NUMBER, value.getType());
        assertEquals(1, value.getNumber(), 0);
    }
}
