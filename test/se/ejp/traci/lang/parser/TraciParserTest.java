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
    public void testColor() throws RecognitionException
    {
        runParser("color [.5, 2.23, .17];");
        assertNoError();
        final Tree node = parseTree.getChild(0);
        assertEquals(TraciParser.COLOR, node.getType());
        assertEquals(3, node.getChildCount());

        runParser("color [.5, 2.23, .17;");
        assertError(MissingTokenException.class);

        runParser("color [.5, 2.23 .17];");
        assertError(MissingTokenException.class);

        runParser("color color [.5, 2.23, .17];");
        assertError(UnwantedTokenException.class);

        runParser("color [.5, 2.23];");
        assertError(MismatchedTokenException.class);

        runParser("color [.5, 2.23, .17, 5];");
        assertError(MismatchedTokenException.class);
        assertError(UnwantedTokenException.class);
    }

    @Test
    public void testVector() throws RecognitionException
    {
        runParser("[.5, 2.23, .17];");
        assertNoError();
        final Tree node = parseTree.getChild(0);
        assertEquals(TraciParser.VECTOR, node.getType());
        assertEquals(4, node.getChildCount());

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
    public void testRef() throws RecognitionException
    {
        runParser("foo;");
        assertNoError();
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
     public void testTransformation() throws RecognitionException
     {
     }
}
