package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;

import org.antlr.runtime.NoViableAltException;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.UnwantedTokenException;
import org.antlr.runtime.tree.Tree;
import org.junit.Test;

public class TraciParserTransformationTest extends TraciParserBase
{
    private void assertTransformation(final Tree node, final String transformation, final int numArgs,
            final boolean hasBlock)
    {
        assertNoError();
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

    public void runTransformation(final String transformation) throws RecognitionException
    {
        runParser(transformation + ";");
        assertTransformation(parseTree.getChild(0), transformation, 0, false);

        runParser(transformation + "();");
        assertTransformation(parseTree.getChild(0), transformation, 0, false);

        runParser(transformation + " { };");
        assertTransformation(parseTree.getChild(0), transformation, 0, true);

        runParser(transformation + " { 17; };");
        assertTransformation(parseTree.getChild(0), transformation, 0, true);

        runParser(transformation + "(23);");
        assertTransformation(parseTree.getChild(0), transformation, 1, false);

        runParser(transformation + " 23;");
        assertTransformation(parseTree.getChild(0), transformation, 1, false);

        runParser(transformation + " [1, 2, 3];");
        assertTransformation(parseTree.getChild(0), transformation, 1, false);

        runParser(transformation + "(23) { };");
        assertTransformation(parseTree.getChild(0), transformation, 1, true);

        runParser(transformation + "(23) { 17; };");
        assertTransformation(parseTree.getChild(0), transformation, 1, true);

        runParser(transformation + "(23, bar);");
        assertTransformation(parseTree.getChild(0), transformation, 2, false);

        runParser(transformation + "(bar, 23) { };");
        assertTransformation(parseTree.getChild(0), transformation, 2, true);

        runParser(transformation + "(23, 1+2) { 17; };");
        assertTransformation(parseTree.getChild(0), transformation, 2, true);

        runParser("foo(" + transformation + ");");
        assertTransformation(parseTree.getChild(0).getChild(1).getChild(0), transformation, 0, false);

        runParser("foo(" + transformation + "());");
        assertTransformation(parseTree.getChild(0).getChild(1).getChild(0), transformation, 0, false);

        runParser("foo(" + transformation + " { });");
        assertTransformation(parseTree.getChild(0).getChild(1).getChild(0), transformation, 0, true);

        runParser("foo(" + transformation + " { 17; });");
        assertTransformation(parseTree.getChild(0).getChild(1).getChild(0), transformation, 0, true);

        runParser("foo(" + transformation + "(23));");
        assertTransformation(parseTree.getChild(0).getChild(1).getChild(0), transformation, 1, false);

        runParser("foo(" + transformation + " 23);");
        assertError(UnwantedTokenException.class);
        assertError(NoViableAltException.class);

        runParser("foo(" + transformation + "(23) { });");
        assertTransformation(parseTree.getChild(0).getChild(1).getChild(0), transformation, 1, true);

        runParser("foo(" + transformation + "(23) { 17; });");
        assertTransformation(parseTree.getChild(0).getChild(1).getChild(0), transformation, 1, true);

        runParser("foo(" + transformation + "(23, bar));");
        assertTransformation(parseTree.getChild(0).getChild(1).getChild(0), transformation, 2, false);

        runParser("foo(" + transformation + "(bar, 23) { });");
        assertTransformation(parseTree.getChild(0).getChild(1).getChild(0), transformation, 2, true);

        runParser("foo(" + transformation + "(23, 1+2) { 17; });");
        assertTransformation(parseTree.getChild(0).getChild(1).getChild(0), transformation, 2, true);
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
    public void testRotate() throws RecognitionException
    {
        runTransformation("rotate");
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
    public void testVecToVeco() throws RecognitionException
    {
        runTransformation("rotVecToVec");
    }
}
