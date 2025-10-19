package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.antlr.runtime.MismatchedTokenException;
import org.antlr.runtime.MissingTokenException;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.UnwantedTokenException;
import org.antlr.runtime.tree.Tree;
import org.junit.Test;

public class TraciParserTest extends TraciParserBase
{
    @Test
    public void testParser() throws RecognitionException
    {
        runParser("17+23;");
        assertNoError();
        assertEquals(TraciParser.BLOCK, parseTree.getType());
        assertEquals(1, parseTree.getChildCount());
        final Tree node = parseTree.getChild(0);
        assertEquals(TraciParser.PLUS_OP, node.getType());
        assertEquals(2, node.getChildCount());
        final Tree left = node.getChild(0);
        final Tree right = node.getChild(1);
        assertEquals(TraciParser.INT, left.getType());
        assertEquals(TraciParser.INT, right.getType());
        assertEquals(0, left.getChildCount());
        assertEquals(0, right.getChildCount());
        assertEquals("17", left.getText());
        assertEquals("23", right.getText());
    }

    @Test
    public void testMismatchedToken() throws RecognitionException
    {
        runParser("17+23");
        assertError(MismatchedTokenException.class);

        runParser("17+23)");
        assertError(MismatchedTokenException.class);
    }

    @Test
    public void testInterior() throws RecognitionException
    {
        runParser("interior(1.1);");
        assertNoError();
        assertEquals(TraciParser.BLOCK, parseTree.getType());
        assertEquals(1, parseTree.getChildCount());
        Tree node = parseTree.getChild(0);
        assertEquals(TraciParser.INTERIOR, node.getType());
        assertEquals(1, node.getChildCount());
        node = node.getChild(0);
        assertEquals(TraciParser.ARGS, node.getType());
        assertEquals(1, node.getChildCount());
        node = node.getChild(0);
        assertEquals(TraciParser.FLOAT, node.getType());

        runParser("interior;");
        assertError(MismatchedTokenException.class);
    }

    @Test
    public void testColor() throws RecognitionException
    {
        runParser("color [.5, 2.23, .17];");
        assertNoError();
        assertEquals(TraciParser.BLOCK, parseTree.getType());
        assertEquals(1, parseTree.getChildCount());
        Tree node = parseTree.getChild(0);
        assertEquals(TraciParser.COLOR, node.getType());
        assertEquals(1, node.getChildCount());
        node = node.getChild(0);
        assertEquals(TraciParser.ARGS, node.getType());
        assertEquals(3, node.getChildCount());

        runParser("color [.5, 2.23, .17, .5];");
        assertNoError();
        assertEquals(TraciParser.BLOCK, parseTree.getType());
        assertEquals(1, parseTree.getChildCount());
        node = parseTree.getChild(0);
        assertEquals(TraciParser.COLOR, node.getType());
        assertEquals(1, node.getChildCount());
        node = node.getChild(0);
        assertEquals(TraciParser.ARGS, node.getType());
        assertEquals(4, node.getChildCount());

        runParser("color [.5, 2.23, .17;");
        assertError(MissingTokenException.class);

        runParser("color [.5, 2.23 .17];");
        assertError(MissingTokenException.class);

        runParser("color color [.5, 2.23, .17];");
        assertError(UnwantedTokenException.class);

        runParser("color [.5, 2.23];");
        assertError(MismatchedTokenException.class);

        runParser("color [.5, 2.23, .17, 5, 7];");
        assertError(MismatchedTokenException.class);
        assertError(UnwantedTokenException.class);
    }

    @Test
    public void testVector() throws RecognitionException
    {
        runParser("[.5, 2.23, .17];");
        assertNoError();
        assertEquals(TraciParser.BLOCK, parseTree.getType());
        assertEquals(1, parseTree.getChildCount());
        Tree node = parseTree.getChild(0);
        assertEquals(TraciParser.VECTOR, node.getType());
        assertEquals(2, node.getChildCount());
        assertEquals(TraciParser.LBRACKET, node.getChild(0).getType());
        node = node.getChild(1);
        assertEquals(TraciParser.ARGS, node.getType());
        assertEquals(3, node.getChildCount());

        runParser("[.5, 2.23, .17;");
        assertError(MissingTokenException.class);

        runParser("[.5, 2.23 .17];");
        assertError(MissingTokenException.class);

        runParser("[.5, 2.23];");
        assertError(MismatchedTokenException.class);

        runParser("[.5, 2.23, .17, 5];");
        assertError(MismatchedTokenException.class);
        assertError(UnwantedTokenException.class);
    }

    @Test
    public void testString() throws RecognitionException
    {
        runParser("\"foo/bar.hej\";");
        assertNoError();
        assertEquals(TraciParser.BLOCK, parseTree.getType());
        assertEquals(1, parseTree.getChildCount());
        final Tree node = parseTree.getChild(0);
        assertEquals(TraciParser.QSTRING, node.getType());
        assertEquals("\"foo/bar.hej\"", node.getText());
        assertEquals(0, node.getChildCount());
    }

    @Test
    public void testRef() throws RecognitionException
    {
        runParser("foo;");
        assertNoError();
        assertEquals(TraciParser.BLOCK, parseTree.getType());
        assertEquals(1, parseTree.getChildCount());
        Tree node = parseTree.getChild(0);
        assertEquals(TraciParser.REF, node.getType());
        assertEquals(1, node.getChildCount());
        node = node.getChild(0);
        assertEquals(TraciParser.ID, node.getType());
        assertEquals("foo", node.getText());
    }

    @Test
    public void testFloat() throws RecognitionException
    {
        runParser(".23;");
        assertNoError();
        assertEquals(TraciParser.BLOCK, parseTree.getType());
        assertEquals(1, parseTree.getChildCount());
        final Tree node = parseTree.getChild(0);
        assertEquals(TraciParser.FLOAT, node.getType());
    }

    @Test
    public void testFibonacciFromFile() throws RecognitionException, IOException
    {
        runParserFile("testcode/fibonacci.traci");
        assertNoError();
    }

    @Test
    public void testFibonacciPreprocessedFile() throws RecognitionException, IOException
    {
        runParserPreprocessedFile("testcode/fibonacci.traci");
        assertNoError();
    }

    @Test
    public void testPrimeCheckerFromFile() throws RecognitionException, IOException
    {
        runParserFile("testcode/prime-checker.traci");
        assertNoError();
    }

    @Test
    public void testPrimeCheckerPreprocessedFile() throws RecognitionException, IOException
    {
        runParserPreprocessedFile("testcode/prime-checker.traci");
        assertNoError();
    }
}
