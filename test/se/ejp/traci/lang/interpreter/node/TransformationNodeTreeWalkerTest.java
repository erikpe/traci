package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.parser.TraciTreeWalkerBase;

public class TransformationNodeTreeWalkerTest extends TraciTreeWalkerBase
{
    private void assertTransformation(final String transformation, final int numArgs, final boolean hasBlock,
            final boolean isWrapped)
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
        assertEquals(transformation, trNode.transformationType.id);
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

    private void runTransformation(final String transformation) throws RecognitionException
    {
        runTreeWalker(transformation + ";");
        assertTransformation(transformation, 0, false, false);

        runTreeWalker(transformation + "();");
        assertTransformation(transformation, 0, false, false);

        runTreeWalker(transformation + " { };");
        assertTransformation(transformation, 0, true, false);

        runTreeWalker(transformation + " { 17; };");
        assertTransformation(transformation, 0, true, false);

        runTreeWalker(transformation + "(23);");
        assertTransformation(transformation, 1, false, false);

        runTreeWalker(transformation + " 23;");
        assertTransformation(transformation, 1, false, false);

        runTreeWalker(transformation + " [1, 2, 3];");
        assertTransformation(transformation, 1, false, false);

        runTreeWalker(transformation + "(23) { };");
        assertTransformation(transformation, 1, true, false);

        runTreeWalker(transformation + "(23) { 17; };");
        assertTransformation(transformation, 1, true, false);

        runTreeWalker(transformation + "(23, bar);");
        assertTransformation(transformation, 2, false, false);

        runTreeWalker(transformation + "(bar, 23) { };");
        assertTransformation(transformation, 2, true, false);

        runTreeWalker(transformation + "(23, 1+2) { 17; };");
        assertTransformation(transformation, 2, true, false);

        runTreeWalker("foo(" + transformation + ");");
        assertTransformation(transformation, 0, false, true);

        runTreeWalker("foo(" + transformation + "());");
        assertTransformation(transformation, 0, false, true);

        runTreeWalker("foo(" + transformation + " { });");
        assertTransformation(transformation, 0, true, true);

        runTreeWalker("foo(" + transformation + " { 17; });");
        assertTransformation(transformation, 0, true, true);

        runTreeWalker("foo(" + transformation + "(23));");
        assertTransformation(transformation, 1, false, true);

        runTreeWalker("foo(" + transformation + "(23) { });");
        assertTransformation(transformation, 1, true, true);

        runTreeWalker("foo(" + transformation + "(23) { 17; });");
        assertTransformation(transformation, 1, true, true);

        runTreeWalker("foo(" + transformation + "(23, bar));");
        assertTransformation(transformation, 2, false, true);

        runTreeWalker("foo(" + transformation + "(bar, 23) { });");
        assertTransformation(transformation, 2, true, true);

        runTreeWalker("foo(" + transformation + "(23, 1+2) { 17; });");
        assertTransformation(transformation, 2, true, true);
}

    @Test
    public void testTranslate() throws RecognitionException
    {
        runTransformation("translate");
    }

    @Test
    public void testScale() throws RecognitionException
    {
        runTransformation("scale");
    }

    @Test
    public void testScaleX() throws RecognitionException
    {
        runTransformation("scalex");
    }

    @Test
    public void testScaleY() throws RecognitionException
    {
        runTransformation("scaley");
    }

    @Test
    public void testScaleZ() throws RecognitionException
    {
        runTransformation("scalez");
    }

    @Test
    public void testRotX() throws RecognitionException
    {
        runTransformation("rotx");
    }

    @Test
    public void testRotY() throws RecognitionException
    {
        runTransformation("roty");
    }

    @Test
    public void testRotZ() throws RecognitionException
    {
        runTransformation("rotz");
    }

    @Test
    public void testAround() throws RecognitionException
    {
        runTransformation("rotAround");
    }

    @Test
    public void testVecToVec() throws RecognitionException
    {
        runTransformation("rotVecToVec");
    }
}
