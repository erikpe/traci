package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.MismatchedTokenException;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
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

    @Test
    public void testParser() throws RecognitionException
    {
        runParser("17+23;");
        assertEquals(0, parserErrors.size());
    }

    @Test
    public void testMismatchedToken() throws RecognitionException
    {
        runParser("17+23");
        assertEquals(1, parserErrors.size());
        assertEquals(MismatchedTokenException.class, parserErrors.get(0).e.getClass());

        runParser("17+23)");
        assertEquals(1, parserErrors.size());
        assertEquals(MismatchedTokenException.class, parserErrors.get(0).e.getClass());
    }
}
