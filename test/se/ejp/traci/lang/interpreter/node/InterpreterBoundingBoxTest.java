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
import se.ejp.traci.model.shape.BoundingBox;

public class InterpreterBoundingBoxTest extends InterpreterBase
{
    @Test
    public void testBoundingBox() throws RecognitionException, InterpreterRuntimeException
    {
        final Transformation eye = Transformations.identity();
        final Transformation t0 = Transformations.translate(1.0, 2.0, 3.0);
        final Transformation t1 = Transformations.scalez(2.23);

        runInterpreter("return bbox;");
        assertEquals(Type.BOUNDING_BOX, value.getType());
        assertEquals(BoundingBox.class, value.getBoundingBox().getClass());
        assertEquals(eye, value.getBoundingBox().getTransformation());

        runInterpreter("return bbox();");
        assertEquals(Type.BOUNDING_BOX, value.getType());
        assertEquals(BoundingBox.class, value.getBoundingBox().getClass());
        assertEquals(eye, value.getBoundingBox().getTransformation());

        runInterpreter("return bbox() { };");
        assertEquals(Type.BOUNDING_BOX, value.getType());
        assertEquals(BoundingBox.class, value.getBoundingBox().getClass());
        assertEquals(eye, value.getBoundingBox().getTransformation());

        runInterpreter("return bbox() { translate [1, 2, 3]; };");
        assertEquals(Type.BOUNDING_BOX, value.getType());
        assertEquals(BoundingBox.class, value.getBoundingBox().getClass());
        assertEquals(t0, value.getBoundingBox().getTransformation());

        runInterpreter("return bbox() { translate [1, 2, 3]; scalez 2.23; };");
        assertEquals(Type.BOUNDING_BOX, value.getType());
        assertEquals(BoundingBox.class, value.getBoundingBox().getClass());
        assertEquals(t0.compose(t1), value.getBoundingBox().getTransformation());

        runInterpreter("return bbox() { translate([1, 2, 3]) { scalez 2.23; }; };");
        assertEquals(Type.BOUNDING_BOX, value.getType());
        assertEquals(BoundingBox.class, value.getBoundingBox().getClass());
        assertEquals(t0.compose(t1), value.getBoundingBox().getTransformation());

        runInterpreter("return bbox([1,2,3], [4,5,6]);");
        assertEquals(Type.BOUNDING_BOX, value.getType());
        assertEquals(BoundingBox.class, value.getBoundingBox().getClass());

        runInterpreter("return bbox([1,2,3], [4,5,6]) { translate [1, 2, 3]; };");
        assertEquals(Type.BOUNDING_BOX, value.getType());
        assertEquals(BoundingBox.class, value.getBoundingBox().getClass());

        runInterpreter("return bbox([1,10,100], [4,14,105]);");
        assertEquals(Type.BOUNDING_BOX, value.getType());
        assertEquals(BoundingBox.class, value.getBoundingBox().getClass());
        {
            final Transformation tmp0 = Transformations.scale(3.0, 4.0, 5.0);
            final Transformation tmp1 = Transformations.translate(1.0, 10.0, 100.0);
            assertEquals(tmp0.compose(tmp1), value.getBoundingBox().getTransformation());
        }

        runInterpreter("return bbox([1,10,100], [4,14,105]) { translate [10,20,30]; };");
        assertEquals(Type.BOUNDING_BOX, value.getType());
        assertEquals(BoundingBox.class, value.getBoundingBox().getClass());
        {
            final Transformation tmp0 = Transformations.scale(3.0, 4.0, 5.0);
            final Transformation tmp1 = Transformations.translate(1.0, 10.0, 100.0);
            final Transformation tmp2 = Transformations.translate(10.0, 20.0, 30.0);
            assertEquals(tmp0.compose(tmp1).compose(tmp2), value.getBoundingBox().getTransformation());
        }

        try
        {
            runInterpreter("return bbox([1,2,3]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("bbox", e.function);
            assertEquals(Collections.singletonList(Type.VECTOR), e.gotArgTypes);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }

        try
        {
            runInterpreter("return bbox([1,2,3], 4, 1<2);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            final List<Type> expectedArgTypes = new ArrayList<Type>();
            expectedArgTypes.add(Type.VECTOR);
            expectedArgTypes.add(Type.NUMBER);
            expectedArgTypes.add(Type.BOOLEAN);
            assertEquals("bbox", e.function);
            assertEquals(expectedArgTypes, e.gotArgTypes);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }
    }
}
