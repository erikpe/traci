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
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.shape.primitive.Box;
import se.ejp.traci.model.shape.primitive.Cylinder;
import se.ejp.traci.model.shape.primitive.Plane;
import se.ejp.traci.model.shape.primitive.Sphere;
import se.ejp.traci.model.shape.primitive.Torus;

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

    @Test
    public void testCylinder() throws RecognitionException, InterpreterRuntimeException
    {
        final Transformation eye = Transformations.identity();
        final Transformation t0 = Transformations.translate(1.0, 2.0, 3.0);
        final Transformation t1 = Transformations.scalez(2.23);

        runInterpreter("return cylinder;");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Cylinder.class, value.getPrimitive().getClass());
        assertEquals(eye, value.getPrimitive().getTransformation());

        runInterpreter("return cylinder();");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Cylinder.class, value.getPrimitive().getClass());
        assertEquals(eye, value.getPrimitive().getTransformation());

        runInterpreter("return cylinder() { };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Cylinder.class, value.getPrimitive().getClass());
        assertEquals(eye, value.getPrimitive().getTransformation());

        runInterpreter("return cylinder() { translate [1, 2, 3]; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Cylinder.class, value.getPrimitive().getClass());
        assertEquals(t0, value.getPrimitive().getTransformation());

        runInterpreter("return cylinder() { translate [1, 2, 3]; scalez 2.23; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Cylinder.class, value.getPrimitive().getClass());
        assertEquals(t0.compose(t1), value.getPrimitive().getTransformation());

        runInterpreter("return cylinder() { translate([1, 2, 3]) { scalez 2.23; }; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Cylinder.class, value.getPrimitive().getClass());
        assertEquals(t0.compose(t1), value.getPrimitive().getTransformation());

        runInterpreter("return cylinder(2.23, [1,2,3], [4,5,6]);");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Cylinder.class, value.getPrimitive().getClass());

        runInterpreter("return cylinder(2.23, [1,2,3], [4,5,6]) { translate [1, 2, 3]; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Cylinder.class, value.getPrimitive().getClass());

        runInterpreter("return cylinder(2.23, [1,10,100], [4,14,105]);");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Cylinder.class, value.getPrimitive().getClass());
        {
            final Vector v0 = Vector.make(1, 10, 100);
            final Vector v1 = Vector.make(4, 14, 105);
            final double length = v1.sub(v0).length();
            final Transformation tmp0 = Transformations.scale(2.23, length, 2.23);
            final Transformation tmp1 = Transformations.rotVecToVec(Vector.UNIT_Y, v1.sub(v0));
            final Transformation tmp2 = Transformations.translate(v0);
            assertEquals(tmp0.compose(tmp1).compose(tmp2), value.getPrimitive().getTransformation());
        }

        runInterpreter("return cylinder(2.23, [1,10,100], [4,14,105]) { translate [10,20,30]; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Cylinder.class, value.getPrimitive().getClass());
        {
            final Vector v0 = Vector.make(1, 10, 100);
            final Vector v1 = Vector.make(4, 14, 105);
            final double length = v1.sub(v0).length();
            final Transformation tmp0 = Transformations.scale(2.23, length, 2.23);
            final Transformation tmp1 = Transformations.rotVecToVec(Vector.UNIT_Y, v1.sub(v0));
            final Transformation tmp2 = Transformations.translate(v0);
            final Transformation tmp3 = Transformations.translate(10.0, 20.0, 30.0);
            assertEquals(tmp0.compose(tmp1).compose(tmp2).compose(tmp3), value.getPrimitive().getTransformation());
        }

        try
        {
            runInterpreter("return cylinder([1,2,3]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("cylinder", e.function);
            assertEquals(Collections.singletonList(Type.VECTOR), e.gotArgTypes);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }

        try
        {
            runInterpreter("return cylinder([1,2,3], 4, 1<2);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            final List<Type> expectedArgTypes = new ArrayList<Type>();
            expectedArgTypes.add(Type.VECTOR);
            expectedArgTypes.add(Type.NUMBER);
            expectedArgTypes.add(Type.BOOLEAN);
            assertEquals("cylinder", e.function);
            assertEquals(expectedArgTypes, e.gotArgTypes);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }

        try
        {
            runInterpreter("return cylinder([1,2,3], [4,5,6]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("cylinder", e.function);
        }

        try
        {
            runInterpreter("return cylinder(1);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("cylinder", e.function);
        }

        try
        {
            runInterpreter("return cylinder(1, 2);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("cylinder", e.function);
        }
    }

    @Test
    public void testPlane() throws RecognitionException, InterpreterRuntimeException
    {
        final Transformation eye = Transformations.identity();
        final Transformation t0 = Transformations.translate(1.0, 2.0, 3.0);
        final Transformation t1 = Transformations.scalez(2.23);

        runInterpreter("return plane;");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Plane.class, value.getPrimitive().getClass());
        assertEquals(eye, value.getPrimitive().getTransformation());

        runInterpreter("return plane();");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Plane.class, value.getPrimitive().getClass());
        assertEquals(eye, value.getPrimitive().getTransformation());

        runInterpreter("return plane() { };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Plane.class, value.getPrimitive().getClass());
        assertEquals(eye, value.getPrimitive().getTransformation());

        runInterpreter("return plane() { translate [1, 2, 3]; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Plane.class, value.getPrimitive().getClass());
        assertEquals(t0, value.getPrimitive().getTransformation());

        runInterpreter("return plane() { translate [1, 2, 3]; scalez 2.23; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Plane.class, value.getPrimitive().getClass());
        assertEquals(t0.compose(t1), value.getPrimitive().getTransformation());

        runInterpreter("return plane() { translate([1, 2, 3]) { scalez 2.23; }; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Plane.class, value.getPrimitive().getClass());
        assertEquals(t0.compose(t1), value.getPrimitive().getTransformation());

        runInterpreter("return plane() { translate [10,20,30]; scalex 2.23; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Plane.class, value.getPrimitive().getClass());
        {
            final Transformation tmp0 = Transformations.translate(10.0, 20.0, 30.0);
            final Transformation tmp1 = Transformations.scalex(2.23);
            assertEquals(tmp0.compose(tmp1), value.getPrimitive().getTransformation());
        }

        try
        {
            runInterpreter("return plane([1,2,3]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("plane", e.function);
            assertEquals(Collections.singletonList(Type.VECTOR), e.gotArgTypes);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }

        try
        {
            runInterpreter("return plane([1,2,3], 4, 1<2);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            final List<Type> expectedArgTypes = new ArrayList<Type>();
            expectedArgTypes.add(Type.VECTOR);
            expectedArgTypes.add(Type.NUMBER);
            expectedArgTypes.add(Type.BOOLEAN);
            assertEquals("plane", e.function);
            assertEquals(expectedArgTypes, e.gotArgTypes);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }

        try
        {
            runInterpreter("return plane([1,2,3], [4,5,6]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("plane", e.function);
        }

        try
        {
            runInterpreter("return plane(2.23, [1,2,3], [4,5,6]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("plane", e.function);
        }

        try
        {
            runInterpreter("return plane(1);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("plane", e.function);
        }

        try
        {
            runInterpreter("return plane(1, 2);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("plane", e.function);
        }
    }

    @Test
    public void testSphere() throws RecognitionException, InterpreterRuntimeException
    {
        final Transformation eye = Transformations.identity();
        final Transformation t0 = Transformations.translate(1.0, 2.0, 3.0);
        final Transformation t1 = Transformations.scalez(2.23);

        runInterpreter("return sphere;");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Sphere.class, value.getPrimitive().getClass());
        assertEquals(eye, value.getPrimitive().getTransformation());

        runInterpreter("return sphere();");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Sphere.class, value.getPrimitive().getClass());
        assertEquals(eye, value.getPrimitive().getTransformation());

        runInterpreter("return sphere() { };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Sphere.class, value.getPrimitive().getClass());
        assertEquals(eye, value.getPrimitive().getTransformation());

        runInterpreter("return sphere() { translate [1, 2, 3]; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Sphere.class, value.getPrimitive().getClass());
        assertEquals(t0, value.getPrimitive().getTransformation());

        runInterpreter("return sphere() { translate [1, 2, 3]; scalez 2.23; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Sphere.class, value.getPrimitive().getClass());
        assertEquals(t0.compose(t1), value.getPrimitive().getTransformation());

        runInterpreter("return sphere() { translate([1, 2, 3]) { scalez 2.23; }; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Sphere.class, value.getPrimitive().getClass());
        assertEquals(t0.compose(t1), value.getPrimitive().getTransformation());

        runInterpreter("return sphere() { translate [10,20,30]; scalex 2.23; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Sphere.class, value.getPrimitive().getClass());
        {
            final Transformation tmp0 = Transformations.translate(10.0, 20.0, 30.0);
            final Transformation tmp1 = Transformations.scalex(2.23);
            assertEquals(tmp0.compose(tmp1), value.getPrimitive().getTransformation());
        }

        runInterpreter("return sphere(2.23);");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Sphere.class, value.getPrimitive().getClass());
        {
            assertEquals(Transformations.scale(2.23), value.getPrimitive().getTransformation());
        }

        runInterpreter("return sphere(2.23) { translate [10,20,30]; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Sphere.class, value.getPrimitive().getClass());
        {
            final Transformation tmp0 = Transformations.scale(2.23);
            final Transformation tmp1 = Transformations.translate(10.0, 20.0, 30.0);
            assertEquals(tmp0.compose(tmp1), value.getPrimitive().getTransformation());
        }

        try
        {
            runInterpreter("return sphere([1,2,3]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("sphere", e.function);
            assertEquals(Collections.singletonList(Type.VECTOR), e.gotArgTypes);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }

        try
        {
            runInterpreter("return sphere([1,2,3], 4, 1<2);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            final List<Type> expectedArgTypes = new ArrayList<Type>();
            expectedArgTypes.add(Type.VECTOR);
            expectedArgTypes.add(Type.NUMBER);
            expectedArgTypes.add(Type.BOOLEAN);
            assertEquals("sphere", e.function);
            assertEquals(expectedArgTypes, e.gotArgTypes);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }

        try
        {
            runInterpreter("return sphere([1,2,3], [4,5,6]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("sphere", e.function);
        }

        try
        {
            runInterpreter("return sphere(2.23, [1,2,3], [4,5,6]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("sphere", e.function);
        }

        try
        {
            runInterpreter("return sphere(1, 2);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("sphere", e.function);
        }
    }

    @Test
    public void testTorus() throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter("return torus(.23);");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Torus.class, value.getPrimitive().getClass());
        assertEquals(.23, ((Torus) value.getPrimitive()).r, 0);
        assertEquals(Transformations.identity(), value.getPrimitive().getTransformation());

        runInterpreter("return torus(.23) { };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Torus.class, value.getPrimitive().getClass());
        assertEquals(.23, ((Torus) value.getPrimitive()).r, 0);
        assertEquals(Transformations.identity(), value.getPrimitive().getTransformation());

        runInterpreter("return torus(.23) { rotz 2.2; translate [10,20,30]; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Torus.class, value.getPrimitive().getClass());
        {
            assertEquals(.23, ((Torus) value.getPrimitive()).r, 0);
            final Transformation tmp0 = Transformations.rotz(2.2);
            final Transformation tmp1 = Transformations.translate(10.0, 20.0, 30.0);
            assertEquals(tmp0.compose(tmp1), value.getPrimitive().getTransformation());
        }

        runInterpreter("return torus(.23, 2);");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Torus.class, value.getPrimitive().getClass());
        assertEquals(.23/2.0, ((Torus) value.getPrimitive()).r, 0);
        assertEquals(Transformations.scale(2.0), value.getPrimitive().getTransformation());

        runInterpreter("return torus(.23, 2) { };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Torus.class, value.getPrimitive().getClass());
        assertEquals(.23/2.0, ((Torus) value.getPrimitive()).r, 0);
        assertEquals(Transformations.scale(2.0), value.getPrimitive().getTransformation());

        runInterpreter("return torus(.23, 2) { translate [10,20,30]; };");
        assertEquals(Type.PRIMITIVE_SHAPE, value.getType());
        assertEquals(Torus.class, value.getPrimitive().getClass());
        {
            assertEquals(.23/2.0, ((Torus) value.getPrimitive()).r, 0);
            final Transformation tmp0 = Transformations.scale(2.0);
            final Transformation tmp1 = Transformations.translate(10.0, 20.0, 30.0);
            assertEquals(tmp0.compose(tmp1), value.getPrimitive().getTransformation());
        }

        try
        {
            runInterpreter("return torus;");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("torus", e.function);
        }

        try
        {
            runInterpreter("return torus([1,2,3]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("torus", e.function);
            assertEquals(Collections.singletonList(Type.VECTOR), e.gotArgTypes);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }

        try
        {
            runInterpreter("return torus([1,2,3], 4, 1<2);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            final List<Type> expectedArgTypes = new ArrayList<Type>();
            expectedArgTypes.add(Type.VECTOR);
            expectedArgTypes.add(Type.NUMBER);
            expectedArgTypes.add(Type.BOOLEAN);
            assertEquals("torus", e.function);
            assertEquals(expectedArgTypes, e.gotArgTypes);
            assertEquals(1, e.includeLocation.fileLocation.row);
            assertEquals(7, e.includeLocation.fileLocation.col);
        }

        try
        {
            runInterpreter("return torus([1,2,3], [4,5,6]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("torus", e.function);
        }
        try
        {
            runInterpreter("return torus(2.23, [1,2,3], [4,5,6]);");
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals("torus", e.function);
        }
    }
}
