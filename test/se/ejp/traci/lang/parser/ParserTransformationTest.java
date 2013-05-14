package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;

import org.antlr.runtime.NoViableAltException;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.UnwantedTokenException;
import org.antlr.runtime.tree.Tree;
import org.junit.Test;

public class ParserTransformationTest extends TraciParserBase
{
    private void assertResult(final String id, final int numArgs, final boolean hasBlock, final boolean isWrapped)
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
        assertEquals(id, node.getText());

        if (hasBlock)
        {
            assertEquals(2, node.getChildCount());
            assertEquals(TraciParser.BLOCK, node.getChild(1).getType());
        }
        else
        {
            assertEquals(1, node.getChildCount());
        }

        assertEquals(TraciParser.ARGS, node.getChild(0).getType());
        assertEquals(numArgs, node.getChild(0).getChildCount());
    }

    private void runTest(final String id) throws RecognitionException
    {
        runParser(id + ";");
        assertResult(id, 0, false, false);

        runParser(id + "();");
        assertResult(id, 0, false, false);

        runParser(id + " { };");
        assertResult(id, 0, true, false);

        runParser(id + " () { };");
        assertResult(id, 0, true, false);

        runParser(id + " { 17; };");
        assertResult(id, 0, true, false);

        runParser(id + "(23);");
        assertResult(id, 1, false, false);

        runParser(id + " 23;");
        assertResult(id, 1, false, false);

        runParser(id + " [1, 2, 3];");
        assertResult(id, 1, false, false);

        runParser(id + "(23) { };");
        assertResult(id, 1, true, false);

        runParser(id + "(23) { 17; };");
        assertResult(id, 1, true, false);

        runParser(id + "(23, bar);");
        assertResult(id, 2, false, false);

        runParser(id + "(bar, 23) { };");
        assertResult(id, 2, true, false);

        runParser(id + "(23, 1+2) { 17; };");
        assertResult(id, 2, true, false);

        runParser("foo(" + id + ");");
        assertResult(id, 0, false, true);

        runParser("foo(" + id + "());");
        assertResult(id, 0, false, true);

        runParser("foo(" + id + " { });");
        assertResult(id, 0, true, true);

        runParser("foo(" + id + " { 17; });");
        assertResult(id, 0, true, true);

        runParser("foo(" + id + "(23));");
        assertResult(id, 1, false, true);

        runParser("foo(" + id + " 23);");
        assertError(UnwantedTokenException.class);
        assertError(NoViableAltException.class);

        runParser("foo(" + id + " [1, 2, 3]);");
        assertError(UnwantedTokenException.class);
        assertError(NoViableAltException.class);

        runParser("foo(" + id + "(23) { });");
        assertResult(id, 1, true, true);

        runParser("foo(" + id + "(23) { 17; });");
        assertResult(id, 1, true, true);

        runParser("foo(" + id + "(23, bar));");
        assertResult(id, 2, false, true);

        runParser("foo(" + id + "(bar, 23) { });");
        assertResult(id, 2, true, true);

        runParser("foo(" + id + "(23, 1+2) { 17; });");
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
