package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.model.shape.Shape;
import se.ejp.traci.model.shape.csg.Csg;
import se.ejp.traci.model.shape.csg.Difference;
import se.ejp.traci.model.shape.csg.Intersection;
import se.ejp.traci.model.shape.csg.Union;
import se.ejp.traci.model.shape.primitive.Box;
import se.ejp.traci.model.shape.primitive.Sphere;

public class InterpreterCsgShapeTest extends InterpreterObjectBase
{
    public InterpreterCsgShapeTest()
    {
        super(Type.CSG_SHAPE);
    }

    private void testCsg(final String id, final Class<? extends Csg> clazz) throws RecognitionException,
            InterpreterRuntimeException
    {
        final String m0 = "box; translate [1, 2, 3]; sphere; translate [10, 20, 30];";
        final String m1 = "translate [1, 2, 3]; bbox; translate [10, 20, 30];";

        final Transformation t0 = Transformations.translate(1.0, 2.0, 3.0);
        final Transformation t1 = Transformations.translate(10.0, 20.0, 30.0);

        for (final Csg csg : runTestsNoArgs(id, clazz, null))
        {
            assertNull(csg.getBoundingBox());
            assertTrue(csg.getShapes().isEmpty());
        }

        for (final Csg csg : runTestsNoArgs(id, clazz, m0))
        {
            assertNull(csg.getBoundingBox());
            final List<Shape> children = csg.getShapes();
            assertEquals(2, children.size());
            assertEquals(children.get(0).getClass(), Box.class);
            final Box box = (Box) children.get(0);
            assertEquals(t0.compose(t1), box.getTransformation());
            assertEquals(children.get(1).getClass(), Sphere.class);
            final Sphere sphere = (Sphere) children.get(1);
            assertEquals(t1, sphere.getTransformation());
        }

        for (final Csg csg : runTestsNoArgs(id, clazz, m1))
        {
            assertNotNull(csg.getBoundingBox());
            assertTrue(csg.getShapes().isEmpty());
            assertEquals(t1, csg.getBoundingBox().getTransformation());
        }
    }

    @Test
    public void testUnion() throws RecognitionException, InterpreterRuntimeException
    {
        testCsg("union", Union.class);
    }

    @Test
    public void testDifference() throws RecognitionException, InterpreterRuntimeException
    {
        testCsg("difference", Difference.class);
    }

    @Test
    public void testIntersection() throws RecognitionException, InterpreterRuntimeException
    {
        testCsg("intersection", Intersection.class);
    }
}
