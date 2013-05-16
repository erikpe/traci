package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import static se.ejp.traci.util.AssertApprox.assertApprox;

import java.util.Collections;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.InterpreterBase;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalArguments;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.math.Transformations;

public class InterpreterTransformationTest extends InterpreterBase
{
    @Test
    public void testIdentity() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return identity;");
        assertEquals(Type.TRANSFORMATION, value.getType());
        assertApprox(Transformations.identity(), value.getTransformation());

        runInterpreter("return identity();");
        assertEquals(Type.TRANSFORMATION, value.getType());
        assertApprox(Transformations.identity(), value.getTransformation());

        runInterpreter("return identity() { };");
        assertEquals(Type.TRANSFORMATION, value.getType());
        assertApprox(Transformations.identity(), value.getTransformation());

        runInterpreter("return identity { identity; };");
        assertEquals(Type.TRANSFORMATION, value.getType());
        assertApprox(Transformations.identity(), value.getTransformation());

        runInterpreter("return identity { rotx(2.23); };");
        assertEquals(Type.TRANSFORMATION, value.getType());
        assertApprox(Transformations.rotx(2.23), value.getTransformation());

        runInterpreter("return identity { rotx(2.23) { }; rotx(-2.23); };");
        assertEquals(Type.TRANSFORMATION, value.getType());
        assertApprox(Transformations.identity(), value.getTransformation());

        try
        {
            runInterpreter("return identity(2.23);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("identity", e.function);
            assertEquals(Collections.singletonList(Type.NUMBER), e.gotArgTypes);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }
    }
}
