package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.InterpreterBase;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalArguments;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.model.shape.primitive.Box;

public class ShapeNodeTest extends InterpreterBase
{
    @Test
    public void testBox() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return box;");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Box.class, value.getPrimitive().getClass());
        assertEquals(Transformations.identity(), value.getPrimitive().getTransformation());

        runInterpreter("return box();");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Box.class, value.getPrimitive().getClass());
        assertEquals(Transformations.identity(), value.getPrimitive().getTransformation());

        runInterpreter("return box() { };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Box.class, value.getPrimitive().getClass());
        assertEquals(Transformations.identity(), value.getPrimitive().getTransformation());

        runInterpreter("return box([1,2,3], [4,5,6]);");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Box.class, value.getPrimitive().getClass());

        runInterpreter("return box([1,2,3], [4,5,6]) { translate [1, 2, 3]; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Box.class, value.getPrimitive().getClass());

        try
        {
            runInterpreter("return box([1,2,3]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("box", e.function);
            assertEquals(Collections.singletonList(Type.VECTOR), e.gotArgTypes);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }

        try
        {
            runInterpreter("return box([1,2,3], 4, 1<2);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            final List<Type> expectedArgTypes = new ArrayList<Type>();
            expectedArgTypes.add(Type.VECTOR);
            expectedArgTypes.add(Type.NUMBER);
            expectedArgTypes.add(Type.BOOLEAN);
            assertEquals("box", e.function);
            assertEquals(expectedArgTypes, e.gotArgTypes);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }
    }
}
