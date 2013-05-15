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
import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.model.shape.primitive.Box;

public class InterpreterPrimitiveShapeTest extends InterpreterBase
{
    @Test
    public void testBox() throws RecognitionException, InterpreterRuntimeException
    {
        final Transformation eye = Transformations.identity();
        final Transformation t0 = Transformations.translate(1.0, 2.0, 3.0);
        final Transformation t1 = Transformations.scalez(2.23);

        runInterpreter("return box;");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Box.class, value.getPrimitive().getClass());
        assertEquals(eye, value.getPrimitive().getTransformation());

        runInterpreter("return box();");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Box.class, value.getPrimitive().getClass());
        assertEquals(eye, value.getPrimitive().getTransformation());

        runInterpreter("return box() { };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Box.class, value.getPrimitive().getClass());
        assertEquals(eye, value.getPrimitive().getTransformation());

        runInterpreter("return box() { translate [1, 2, 3]; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Box.class, value.getPrimitive().getClass());
        assertEquals(t0, value.getPrimitive().getTransformation());

        runInterpreter("return box() { translate [1, 2, 3]; scalez 2.23; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Box.class, value.getPrimitive().getClass());
        assertEquals(t0.compose(t1), value.getPrimitive().getTransformation());

        runInterpreter("return box() { translate([1, 2, 3]) { scalez 2.23; }; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Box.class, value.getPrimitive().getClass());
        assertEquals(t0.compose(t1), value.getPrimitive().getTransformation());

        runInterpreter("return box([1,2,3], [4,5,6]);");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Box.class, value.getPrimitive().getClass());

        runInterpreter("return box([1,2,3], [4,5,6]) { translate [1, 2, 3]; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Box.class, value.getPrimitive().getClass());

        runInterpreter("return box([1,10,100], [4,14,105]);");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Box.class, value.getPrimitive().getClass());
        {
            final Transformation tmp0 = Transformations.scale(3.0, 4.0, 5.0);
            final Transformation tmp1 = Transformations.translate(1.0, 10.0, 100.0);
            assertEquals(tmp0.compose(tmp1), value.getPrimitive().getTransformation());
        }

        runInterpreter("return box([1,10,100], [4,14,105]) { translate [10,20,30]; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Box.class, value.getPrimitive().getClass());
        {
            final Transformation tmp0 = Transformations.scale(3.0, 4.0, 5.0);
            final Transformation tmp1 = Transformations.translate(1.0, 10.0, 100.0);
            final Transformation tmp2 = Transformations.translate(10.0, 20.0, 30.0);
            assertEquals(tmp0.compose(tmp1).compose(tmp2), value.getPrimitive().getTransformation());
        }

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
