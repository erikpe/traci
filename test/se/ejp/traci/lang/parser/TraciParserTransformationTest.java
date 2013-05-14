package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;

import org.antlr.runtime.NoViableAltException;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.UnwantedTokenException;
import org.antlr.runtime.tree.Tree;
import org.junit.Test;

public class TraciParserTransformationTest extends TraciParserBase
{
    private void assertTransformation(final String transformation, final int numArgs, final boolean hasBlock,
            final boolean isWrapped)
    {
        assertNoError();
        assertEquals(TraciParser.BLOCK, parseTree.getType());
        assertEquals(1, parseTree.getChildCount());
        Tree node = parseTree.getChild(0);

        if (isWrapped)
        {
            assertEquals(TraciParser.FUNCALL, node.getType());
            assertEquals(2, node.getChildCount());
            assertEquals(TraciParser.ID, node.getChild(0).getType());
            assertEquals("foo", node.getChild(0).getText());
            assertEquals(TraciParser.ARGS, node.getChild(1).getType());
            assertEquals(1, node.getChild(1).getChildCount());
            node = node.getChild(1).getChild(0);
        }

        assertEquals(TraciParser.TRANSFORMATION, node.getType());
        assertEquals(transformation, node.getText());
        assertEquals(TraciParser.ARGS, node.getChild(0).getType());
        assertEquals(numArgs, node.getChild(0).getChildCount());

        if (hasBlock)
        {
            assertEquals(2, node.getChildCount());
            assertEquals(TraciParser.BLOCK, node.getChild(1).getType());
        }
        else
        {
            assertEquals(1, node.getChildCount());
        }
    }

    private void runTransformation(final String transformation) throws RecognitionException
    {
        runParser(transformation + ";");
        assertTransformation(transformation, 0, false, false);

        runParser(transformation + "();");
        assertTransformation(transformation, 0, false, false);

        runParser(transformation + " { };");
        assertTransformation(transformation, 0, true, false);

        runParser(transformation + " { 17; };");
        assertTransformation(transformation, 0, true, false);

        runParser(transformation + "(23);");
        assertTransformation(transformation, 1, false, false);

        runParser(transformation + " 23;");
        assertTransformation(transformation, 1, false, false);

        runParser(transformation + " [1, 2, 3];");
        assertTransformation(transformation, 1, false, false);

        runParser(transformation + "(23) { };");
        assertTransformation(transformation, 1, true, false);

        runParser(transformation + "(23) { 17; };");
        assertTransformation(transformation, 1, true, false);

        runParser(transformation + "(23, bar);");
        assertTransformation(transformation, 2, false, false);

        runParser(transformation + "(bar, 23) { };");
        assertTransformation(transformation, 2, true, false);

        runParser(transformation + "(23, 1+2) { 17; };");
        assertTransformation(transformation, 2, true, false);

        runParser("foo(" + transformation + ");");
        assertTransformation(transformation, 0, false, true);

        runParser("foo(" + transformation + "());");
        assertTransformation(transformation, 0, false, true);

        runParser("foo(" + transformation + " { });");
        assertTransformation(transformation, 0, true, true);

        runParser("foo(" + transformation + " { 17; });");
        assertTransformation(transformation, 0, true, true);

        runParser("foo(" + transformation + "(23));");
        assertTransformation(transformation, 1, false, true);

        runParser("foo(" + transformation + " 23);");
        assertError(UnwantedTokenException.class);
        assertError(NoViableAltException.class);

        runParser("foo(" + transformation + " [1, 2, 3]);");
        assertError(UnwantedTokenException.class);
        assertError(NoViableAltException.class);

        runParser("foo(" + transformation + "(23) { });");
        assertTransformation(transformation, 1, true, true);

        runParser("foo(" + transformation + "(23) { 17; });");
        assertTransformation(transformation, 1, true, true);

        runParser("foo(" + transformation + "(23, bar));");
        assertTransformation(transformation, 2, false, true);

        runParser("foo(" + transformation + "(bar, 23) { });");
        assertTransformation(transformation, 2, true, true);

        runParser("foo(" + transformation + "(23, 1+2) { 17; });");
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
