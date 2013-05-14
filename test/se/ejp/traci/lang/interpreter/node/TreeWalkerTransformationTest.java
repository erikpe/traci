package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.parser.TraciTreeWalkerBase;

public class TreeWalkerTransformationTest extends TraciTreeWalkerBase
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

        assertEquals(TransformationNode.class, node.getClass());
        final TransformationNode trNode = (TransformationNode) node;
        assertEquals(id, trNode.transformationType.id);
        assertEquals(numArgs, trNode.argNodes.size());

        if (hasBlock)
        {
            assertNotNull(trNode.blockNode);
        }
        else
        {
            assertNull(trNode.blockNode);
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

        runTreeWalker(id + " 23;");
        assertResult(id, 1, false, false);

        runTreeWalker(id + " [1, 2, 3];");
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
    public void testTranslate() throws RecognitionException
    {
        runTest("translate");
    }

    @Test
    public void testScale() throws RecognitionException
    {
        runTest("scale");
    }

    @Test
    public void testScaleX() throws RecognitionException
    {
        runTest("scalex");
    }

    @Test
    public void testScaleY() throws RecognitionException
    {
        runTest("scaley");
    }

    @Test
    public void testScaleZ() throws RecognitionException
    {
        runTest("scalez");
    }

    @Test
    public void testRotX() throws RecognitionException
    {
        runTest("rotx");
    }

    @Test
    public void testRotY() throws RecognitionException
    {
        runTest("roty");
    }

    @Test
    public void testRotZ() throws RecognitionException
    {
        runTest("rotz");
    }

    @Test
    public void testAround() throws RecognitionException
    {
        runTest("rotAround");
    }

    @Test
    public void testVecToVec() throws RecognitionException
    {
        runTest("rotVecToVec");
    }
}
