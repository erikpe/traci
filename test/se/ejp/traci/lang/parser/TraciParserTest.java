package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.MismatchedTokenException;
import org.antlr.runtime.MissingTokenException;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.UnwantedTokenException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.Tree;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class TraciParserTest
{
    private TraciParser parser = null;
    private CommonTree parseTree = null;
    private List<ParseError> parserErrors = null;

    @Before
    public void setUp()
    {
    }

    @After
    public void tearDown()
    {
        parser = null;
        parseTree = null;
        parserErrors = null;
    }

    private void runParser(final String code) throws RecognitionException
    {
        final TraciLexer lexer = new TraciLexer(new ANTLRStringStream(code));
        parser = new TraciParser(new CommonTokenStream(lexer));
        parserErrors = new ArrayList<ParseError>();

        try
        {
            parseTree = (CommonTree) parser.scene().getTree();
        }
        finally
        {
            assertFalse(lexer.getLexerErrors().iterator().hasNext());
            for (final ParseError error : parser.getParseErrors())
            {
                parserErrors.add(error);
            }
        }
    }

    private void assertNoError()
    {
        assertEquals(0, parserErrors.size());
    }

    private void assertError(final Class<? extends RecognitionException> clazz)
    {
        assertEquals(1, parserErrors.size());
        assertEquals(clazz, parserErrors.get(0).e.getClass());
    }

    private void assertAllErrors(final Class<? extends RecognitionException> ... errorClasses)
    {
        final Set<Class<? extends RecognitionException>> expectedErrors =
                new HashSet<Class<? extends RecognitionException>>(Arrays.asList(errorClasses));
        final Set<Class<? extends RecognitionException>> encounteredErrors =
                new HashSet<Class<? extends RecognitionException>>();

        for (final ParseError error : parserErrors)
        {
            encounteredErrors.add(error.e.getClass());
        }

        assertEquals(expectedErrors, encounteredErrors);
    }

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
    @SuppressWarnings("unchecked")
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
        assertAllErrors(MismatchedTokenException.class, UnwantedTokenException.class);
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testVector() throws RecognitionException
    {
        runParser("[.5, 2.23, .17];");
        assertNoError();
        final Tree node = parseTree.getChild(0);
        assertEquals(TraciParser.VECTOR, node.getType());
        assertEquals(3, node.getChildCount());

        runParser("[.5, 2.23, .17;");
        assertError(MissingTokenException.class);

        runParser("[.5, 2.23 .17];");
        assertError(MissingTokenException.class);

        runParser("[.5, 2.23];");
        assertError(MismatchedTokenException.class);

        runParser("[.5, 2.23, .17, 5];");
        assertAllErrors(MismatchedTokenException.class, UnwantedTokenException.class);
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
}
