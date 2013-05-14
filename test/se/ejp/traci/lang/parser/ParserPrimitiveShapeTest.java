package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;

import org.antlr.runtime.NoViableAltException;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.UnwantedTokenException;
import org.antlr.runtime.tree.Tree;
import org.junit.Test;

public class ParserPrimitiveShapeTest extends TraciParserBase
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

        assertEquals(TraciParser.PRIMITIVE_SHAPE, node.getType());
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
        assertError(NoViableAltException.class);

        runParser(id + " [1, 2, 3];");
        assertError(NoViableAltException.class);

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
