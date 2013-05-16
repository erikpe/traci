package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.parser.TraciTreeWalkerBase;
import se.ejp.traci.model.shape.primitive.Primitive;

public class TreeWalkerPrimitiveShapeTest extends TraciTreeWalkerBase
{
    private void assertResult(final String id, final int numArgs, final boolean hasBlock, final boolean isWrapped)
    {
        assertEquals(1, rootNode.getStatements().size());
        TraciNode node = rootNode.getStatements().get(0);

        if (isWrapped)
        {
            assertEquals(FunctionCallNode.class, node.getClass());
            final FunctionCallNode funCallNode = (FunctionCallNode) node;
            assertEquals("foo", funCallNode.id);
            assertEquals(1, funCallNode.argNodes.size());
            assertNull(funCallNode.blockNode);
            node = funCallNode.argNodes.get(0);
        }

        assertEquals(ObjectNode.class, node.getClass());
        final ObjectNode objectNode = (ObjectNode) node;
        assertEquals(id, objectNode.objectType.id);
        assertTrue(Primitive.class.isAssignableFrom(objectNode.objectType.clazz));
        assertEquals(numArgs, objectNode.argNodes.size());

        if (hasBlock)
        {
            assertNotNull(objectNode.blockNode);
        }
        else
        {
            assertNull(objectNode.blockNode);
        }
    }

    private void runTest(final String id) throws RecognitionException
    {
        runTreeWalker(id + ";");
        assertResult(id, 0, false, false);

        runTreeWalker(id + "();");
        assertResult(id, 0, false, false);

        runTreeWalker(id + " { };");
        assertResult(id, 0, true, false);

        runTreeWalker(id + " () { };");
        assertResult(id, 0, true, false);

        runTreeWalker(id + " { 17; };");
        assertResult(id, 0, true, false);

        runTreeWalker(id + "(23);");
        assertResult(id, 1, false, false);

        runTreeWalker(id + "(23) { };");
        assertResult(id, 1, true, false);

        runTreeWalker(id + "(23) { 17; };");
        assertResult(id, 1, true, false);

        runTreeWalker(id + "(23, bar);");
        assertResult(id, 2, false, false);

        runTreeWalker(id + "(bar, 23) { };");
        assertResult(id, 2, true, false);

        runTreeWalker(id + "(23, 1+2) { 17; };");
        assertResult(id, 2, true, false);

        runTreeWalker("foo(" + id + ");");
        assertResult(id, 0, false, true);

        runTreeWalker("foo(" + id + "());");
        assertResult(id, 0, false, true);

        runTreeWalker("foo(" + id + " { });");
        assertResult(id, 0, true, true);

        runTreeWalker("foo(" + id + " () { });");
        assertResult(id, 0, true, true);

        runTreeWalker("foo(" + id + " { 17; });");
        assertResult(id, 0, true, true);

        runTreeWalker("foo(" + id + "(23));");
        assertResult(id, 1, false, true);

        runTreeWalker("foo(" + id + "(23) { });");
        assertResult(id, 1, true, true);

        runTreeWalker("foo(" + id + "(23) { 17; });");
        assertResult(id, 1, true, true);

        runTreeWalker("foo(" + id + "(23, bar));");
        assertResult(id, 2, false, true);

        runTreeWalker("foo(" + id + "(bar, 23) { });");
        assertResult(id, 2, true, true);

        runTreeWalker("foo(" + id + "(23, 1+2) { 17; });");
        assertResult(id, 2, true, true);
    }

    @Test
    public void testBox() throws RecognitionException
    {
        runTest("box");
    }

    @Test
    public void testCylinder() throws RecognitionException
    {
        runTest("cylinder");
    }

    @Test
    public void testPlane() throws RecognitionException
    {
        runTest("plane");
    }

    @Test
    public void testSphere() throws RecognitionException
    {
        runTest("sphere");
    }

    @Test
    public void testTorus() throws RecognitionException
    {
        runTest("torus");
    }
}
