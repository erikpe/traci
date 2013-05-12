package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.Collections;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.InterpreterBase;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalArgumentType;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.math.Vector;

public class VectorNodeInterpreterTest extends InterpreterBase
{
    @Test
    public void testVector() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return [.5, 2.23, -2];");
        assertEquals(Type.VECTOR, value.getType());
        assertEquals(Vector.make(.5, 2.23, -2), value.getVector());
    }

    @Test
    public void testIllegalArgument() throws RecognitionException, InterpreterRuntimeException
    {
        try
        {
            runInterpreter("return [.5, 1<2, -2];");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("vector", e.function);
            assertEquals(Collections.singleton(Type.NUMBER), e.expectedArgType);
            assertEquals(Type.BOOLEAN, e.gotArgType);
            assertEquals(2, e.argIndex);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }

        try
        {
            runInterpreter("return [[1,2,3], 1, -2];");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArgumentType e)
        {
            assertEquals("vector", e.function);
            assertEquals(Collections.singleton(Type.NUMBER), e.expectedArgType);
            assertEquals(Type.VECTOR, e.gotArgType);
            assertEquals(1, e.argIndex);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }
    }
}
