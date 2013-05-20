package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.InterpreterBase;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalArguments;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.shape.primitive.Box;
import se.ejp.traci.model.shape.primitive.Cylinder;
import se.ejp.traci.model.shape.primitive.Plane;
import se.ejp.traci.model.shape.primitive.Primitive;
import se.ejp.traci.model.shape.primitive.Sphere;
import se.ejp.traci.model.shape.primitive.Torus;

public class InterpreterPrimitiveShapeTest extends InterpreterBase
{
    private static final Transformation EYE = Transformations.identity();

    @SuppressWarnings("unchecked")
    private <P extends Primitive> P run(final String code, final Class<P> clazz, final Transformation tr)
            throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter(code);
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(clazz, value.getPrimitive().getClass());
        assertEquals(tr, value.getPrimitive().getTransformation());
        return (P) value.getPrimitive();
    }

    private void runFail(final String code, final String id, final Class<? extends InterpreterRuntimeException> eClass)
            throws RecognitionException
    {
        try
        {
            runInterpreter(code);
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals(eClass, e.getClass());
            assertEquals(id, e.function);
        }
        catch (final InterpreterRuntimeException e)
        {
            assertEquals(eClass, e.getClass());
        }
    }

    private List<String> getSnippets(final String id, final String args, final String modifiers)
    {
        final List<String> snippets = new ArrayList<String>();

        if (args == null && modifiers == null)
        {
            snippets.add("return " + id + ";");
            snippets.add("return " + id + "();");
            snippets.add("return " + id + " { };");
            snippets.add("return " + id + "() { };");
            snippets.add("val = " + id + "; return val;");
            snippets.add("val = " + id + "(); return val;");
            snippets.add("val = " + id + " { }; return val;");
            snippets.add("val = " + id + "() { }; return val;");
        }
        else if (args != null && modifiers == null)
        {
            snippets.add("return " + id + "(" + args + ");");
            snippets.add("return " + id + "(" + args + ") { };");
            snippets.add("val = " + id + "(" + args + "); return val;");
            snippets.add("val = " + id + "(" + args + ") { }; return val;");
        }
        else if (args == null && modifiers != null)
        {
            snippets.add("return " + id + " { " + modifiers + " };");
            snippets.add("return " + id + "() { " + modifiers + " };");
            snippets.add("val = " + id + " { " + modifiers + " }; return val;");
            snippets.add("val = " + id + "() { " + modifiers + " }; return val;");
            snippets.add("val = " + id + "; return val { " + modifiers + " };");
            snippets.add("val = " + id + "(); return val { " + modifiers + " };");
        }
        else if (args != null && modifiers != null)
        {
            snippets.add("return " + id + "(" + args + ") { " + modifiers + " };");
            snippets.add("val = " + id + "(" + args + ") { " + modifiers + " }; return val;");
            snippets.add("val = " + id + "(" + args + "); return val { " + modifiers + " };");
        }

        return snippets;
    }

    private <P extends Primitive> List<P> runTests(final String id, final Class<P> clazz, final String args,
            final String modifiers, final Transformation tr) throws RecognitionException,
            InterpreterRuntimeException
    {
        final List<P> res = new ArrayList<P>();
        for (final String code : getSnippets(id, args, modifiers))
        {
            res.add(run(code, clazz, tr));
        }
        return res;
    }

    private void runTestsFail(final Class<? extends InterpreterRuntimeException> eClass, final String id,
            final String args, final String modifiers) throws RecognitionException
    {
        for (final String code : getSnippets(id, args, modifiers))
        {
            runFail(code, id, eClass);
        }
    }

    @Test
    public void testBox() throws RecognitionException, InterpreterRuntimeException
    {
        final String args = "[1.0, 10.0, 100.0], [4.0, 14.0, 105.0]";
        final String modifiers = "translate([10.0, 20.0, 30.0]); scale(.5);";

        final Transformation tr0 = Transformations.scale(3.0, 4.0, 5.0);
        final Transformation tr1 = Transformations.translate(1.0, 10.0, 100.0);
        final Transformation tr2 = Transformations.translate(10.0, 20.0, 30.0);
        final Transformation tr3 = Transformations.scale(0.5);

        runTests("box", Box.class, null, null,      EYE);
        runTests("box", Box.class, args, null,      tr0.compose(tr1));
        runTests("box", Box.class, null, modifiers, tr2.compose(tr3));
        runTests("box", Box.class, args, modifiers, tr0.compose(tr1).compose(tr2).compose(tr3));

        runTestsFail(InterpreterIllegalArguments.class, "box", "[1.0, 10.0, 100.0]", null);
        runTestsFail(InterpreterIllegalArguments.class, "box", "[1.0, 10.0, 100.0], 23", null);
    }

    @Test
    public void testCylinder() throws RecognitionException, InterpreterRuntimeException
    {
        final String args = "2.23, [1.0, 10.0, 100.0], [4.0, 14.0, 105.0]";
        final String modifiers = "translate([10.0, 20.0, 30.0]); scale(.5);";

        final Vector v0 = Vector.make(1.0, 10.0, 100.0);
        final Vector v1 = Vector.make(4.0, 14.0, 105.0);
        final double length = v1.sub(v0).length();

        final Transformation tr0 = Transformations.scale(2.23, length, 2.23);
        final Transformation tr1 = Transformations.rotVecToVec(Vector.UNIT_Y, v1.sub(v0));
        final Transformation tr2 = Transformations.translate(v0);
        final Transformation tr3 = Transformations.translate(10.0, 20.0, 30.0);
        final Transformation tr4 = Transformations.scale(0.5);

        runTests("cylinder", Cylinder.class, null, null,      EYE);
        runTests("cylinder", Cylinder.class, args, null,      tr0.compose(tr1).compose(tr2));
        runTests("cylinder", Cylinder.class, null, modifiers, tr3.compose(tr4));
        runTests("cylinder", Cylinder.class, args, modifiers, tr0.compose(tr1).compose(tr2).compose(tr3).compose(tr4));

        runTestsFail(InterpreterIllegalArguments.class, "cylinder", "[1.0, 10.0, 100.0], [4.0, 14.0, 105.0]", null);
        runTestsFail(InterpreterIllegalArguments.class, "cylinder", "[1.0, 10.0, 100.0], [4.0, 14.0, 105.0], 23", null);
    }

    @Test
    public void testPlane() throws RecognitionException, InterpreterRuntimeException
    {
        final String modifiers = "translate([10.0, 20.0, 30.0]); rotx(2.23);";

        final Transformation tr0 = Transformations.translate(10.0, 20.0, 30.0);
        final Transformation tr1 = Transformations.rotx(2.23);

        runTests("plane", Plane.class, null, null,      EYE);
        runTests("plane", Plane.class, null, modifiers, tr0.compose(tr1));

        runTestsFail(InterpreterIllegalArguments.class, "plane", "2.23", null);
    }

    @Test
    public void testSphere() throws RecognitionException, InterpreterRuntimeException
    {
        final String args = "2.23";
        final String modifiers = "translate([10.0, 20.0, 30.0]); scale(.5);";

        final Transformation tr0 = Transformations.scale(2.23);
        final Transformation tr1 = Transformations.translate(10.0, 20.0, 30.0);
        final Transformation tr2 = Transformations.scale(0.5);

        runTests("sphere", Sphere.class, null, null,      EYE);
        runTests("sphere", Sphere.class, args, null,      tr0);
        runTests("sphere", Sphere.class, null, modifiers, tr1.compose(tr2));
        runTests("sphere", Sphere.class, args, modifiers, tr0.compose(tr1).compose(tr2));

        runTestsFail(InterpreterIllegalArguments.class, "sphere", "[1.0, 10.0, 100.0]", null);
        runTestsFail(InterpreterIllegalArguments.class, "sphere", "17, 23", null);
    }

    @Test
    public void testTorus() throws RecognitionException, InterpreterRuntimeException
    {
        final String args0 = "0.23";
        final String args1 = "0.23, 2.5";
        final String modifiers = "translate([10.0, 20.0, 30.0]); scale(.5);";

        final Transformation tr0 = Transformations.scale(2.5);
        final Transformation tr1 = Transformations.translate(10.0, 20.0, 30.0);
        final Transformation tr2 = Transformations.scale(0.5);

        final List<Torus> res0 = new ArrayList<Torus>();
        res0.addAll(runTests("torus", Torus.class, args0, null,      EYE));
        res0.addAll(runTests("torus", Torus.class, args0, modifiers, tr1.compose(tr2)));
        for (final Torus torus: res0)
        {
            assertEquals(0.23, torus.smallRadius(), 0);
        }

        final List<Torus> res1 = new ArrayList<Torus>();
        res1.addAll(runTests("torus", Torus.class, args1, null,      tr0));
        res1.addAll(runTests("torus", Torus.class, args1, modifiers, tr0.compose(tr1).compose(tr2)));
        for (final Torus torus : res1)
        {
            assertEquals(0.23 / 2.5, torus.smallRadius(), 0);
        }

        runTestsFail(InterpreterIllegalArguments.class, "torus", null, null);
        runTestsFail(InterpreterIllegalArguments.class, "torus", "[1.0, 10.0, 100.0]", null);
        runTestsFail(InterpreterIllegalArguments.class, "torus", ".23, 1<2", null);
    }
}
