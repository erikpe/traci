package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;

import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.Tree;
import org.junit.Test;

public class ParserCsgShapeTest extends TraciParserBase
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

        assertEquals(TraciParser.CSG_SHAPE, node.getType());
        assertEquals(id, node.getText());

        if (hasBlock)
        {
            assertEquals(1, node.getChildCount());
            assertEquals(TraciParser.BLOCK, node.getChild(0).getType());
        }
        else
        {
            assertEquals(0, node.getChildCount());
        }
    }

    private void runTest(final String id) throws RecognitionException
    {
        runParser(id + ";");
        assertResult(id, 0, false, false);

        runParser(id + "();");
        assertError();

        runParser(id + " { };");
        assertResult(id, 0, true, false);

        runParser(id + " () { };");
        assertError();

        runParser(id + " { 17; };");
        assertResult(id, 0, true, false);

        runParser(id + "(23);");
        assertError();

        runParser(id + " 23;");
        assertError();

        runParser(id + " [1, 2, 3];");
        assertError();

        runParser(id + "(23) { };");
        assertError();

        runParser(id + "(23) { 17; };");
        assertError();

        runParser(id + "(23, bar);");
        assertError();

        runParser(id + "(bar, 23) { };");
        assertError();

        runParser(id + "(23, 1+2) { 17; };");
        assertError();

        runParser("foo(" + id + ");");
        assertResult(id, 0, false, true);

        runParser("foo(" + id + "());");
        assertError();

        runParser("foo(" + id + " { });");
        assertResult(id, 0, true, true);

        runParser("foo(" + id + " () { });");
        assertError();

        runParser("foo(" + id + " { 17; });");
        assertResult(id, 0, true, true);

        runParser("foo(" + id + "(23));");
        assertError();

        runParser("foo(" + id + " 23);");
        assertError();

        runParser("foo(" + id + " [1, 2, 3]);");
        assertError();

        runParser("foo(" + id + "(23) { });");
        assertError();

        runParser("foo(" + id + "(23) { 17; });");
        assertError();

        runParser("foo(" + id + "(23, bar));");
        assertError();

        runParser("foo(" + id + "(bar, 23) { });");
        assertError();

        runParser("foo(" + id + "(23, 1+2) { 17; });");
        assertError();
    }

    @Test
    public void testUnion() throws RecognitionException
    {
        runTest("union");
    }

    @Test
    public void testDifference() throws RecognitionException
    {
        runTest("difference");
    }

    @Test
    public void testIntersection() throws RecognitionException
    {
        runTest("intersection");
    }
}
