package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.util.ArrayList;
import java.util.List;

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
    TraciParser parser = null;
    CommonTree parseTree = null;
    List<ParseError> parserErrors = null;

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
    }
}
